use crate::grammar::uuid::hash_id_value_u64;
use crate::types;
use crate::types::*;
use regex::Regex;

use super::create_defined_scanner_name;
use super::create_production_uuid_name;
use super::create_scanner_name;
use super::data::ast;
use super::data::ast::ASTNode;
use super::data::ast::ASTNodeTraits;
use super::get_closure;
use super::get_production_plain_name;
use super::get_production_start_items;
use super::get_uuid_grammar_name;
use super::is_production_recursive;
use super::parse;
use super::parse::compile_grammar_ast;

use std::collections::btree_map;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fs::read;
use std::num::NonZeroUsize;
use std::path::PathBuf;
use std::sync::Mutex;
use std::thread::{self};
use std::vec;

/// Compile a complete grammar given a file path to a root *.hcg file.
pub fn compile_from_path(
    root_grammar_absolute_path: &PathBuf,
    number_of_threads: usize,
) -> (Option<GrammarStore>, Vec<ParseError>)
{
    let mut pending_grammar_paths = Mutex::new(VecDeque::<PathBuf>::new());

    let mut claimed_grammar_paths = Mutex::new(HashSet::<PathBuf>::new());

    // Pending Work, Claimed Work, Completed Work
    let mut work_verifier = Mutex::new(WorkVerifier::new(1));

    pending_grammar_paths
        .lock()
        .unwrap()
        .push_back(root_grammar_absolute_path.to_owned());

    let number_of_threads = std::thread::available_parallelism()
        .unwrap_or(NonZeroUsize::new(1).unwrap())
        .get();

    let mut results = thread::scope(|s| {
        [0..number_of_threads]
            .into_iter()
            .map(|i| {
                s.spawn(|| {
                    let mut raw_grammars = vec![];
                    let mut errors = vec![];

                    loop {
                        match {
                            let val = pending_grammar_paths
                                .lock()
                                .unwrap()
                                .pop_front();

                            val
                        } {
                            Some(path) => {
                                let have_work = {
                                    let result = claimed_grammar_paths
                                        .lock()
                                        .unwrap()
                                        .insert(path.to_owned());

                                    {
                                        let mut work_verifier =
                                            work_verifier.lock().unwrap();

                                        if result {
                                            work_verifier
                                                .start_one_unit_of_work()
                                        } else {
                                            work_verifier
                                                .skip_one_unit_of_work()
                                        }
                                    }

                                    result
                                };

                                if have_work {
                                    let (grammar, mut new_errors) =
                                        compile_file_path(&path);

                                    errors.append(&mut new_errors);

                                    let mut work_verifier =
                                        work_verifier.lock().unwrap();

                                    work_verifier.complete_one_unit_of_work();

                                    if let Some(grammar) = grammar {
                                        work_verifier.add_units_of_work(
                                            grammar.imports.len() as u32,
                                        );

                                        for (_, b) in grammar.imports.values() {
                                            pending_grammar_paths
                                                .lock()
                                                .unwrap()
                                                .push_back(b.to_owned());
                                        }
                                        raw_grammars.push(grammar);
                                    }
                                };
                            }
                            None => {
                                let res = {
                                    let work_verifier =
                                        work_verifier.lock().unwrap();

                                    work_verifier.is_complete()
                                };

                                if res {
                                    break;
                                }
                            }
                        }
                    }

                    (raw_grammars, errors)
                })
            })
            .map(|s| s.join().unwrap())
            .collect::<Vec<_>>()
            .into_iter()
            .collect::<VecDeque<_>>()
    });

    let mut errors = vec![];
    let mut grammars = VecDeque::new();

    for (mut grammar_results, mut error_results) in results.into_iter() {
        grammars
            .append(&mut grammar_results.into_iter().collect::<VecDeque<_>>());
        errors.append(&mut error_results);
    }

    if grammars.is_empty() {
        (None, errors)
    } else {
        let mut grammar = grammars.pop_front().unwrap();

        merge_grammars(
            &mut grammar,
            &grammars.into_iter().collect::<Vec<_>>(),
            &mut errors,
        );

        if errors.is_empty() {
            let grammar =
                finalize_grammar(grammar, &mut errors, number_of_threads);
            (Some(grammar), errors)
        } else {
            (Some(grammar), errors)
        }
    }
}

/// Compile a grammar defined in a string
pub fn compile_from_string(
    string: &str,
    absolute_path: &PathBuf,
) -> (Option<GrammarStore>, Vec<ParseError>)
{
    match compile_grammar_ast(Vec::from(string.as_bytes())) {
        Ok(grammar) => {
            let (grammar, mut errors) =
                pre_process_grammar(&grammar, absolute_path);

            let grammar = finalize_grammar(grammar, &mut errors, 1);

            (Some(grammar), errors)
        }
        Err(err) => (None, vec![err]),
    }
}

fn compile_file_path(
    absolute_path: &PathBuf,
) -> (Option<GrammarStore>, Vec<ParseError>)
{
    match read(absolute_path) {
        Ok(buffer) => match compile_grammar_ast(buffer) {
            Ok(grammar) => {
                let (grammar, errors) =
                    pre_process_grammar(&grammar, absolute_path);

                (Some(grammar), errors)
            }
            Err(err) => (None, vec![err]),
        },
        Err(err) => (None, vec![ParseError::IO_ERROR(err)]),
    }
}

/// Create scanner productions, adds ids to tokens, creates cache
/// data.

fn finalize_grammar(
    mut grammar: GrammarStore,
    mut errors: &mut [ParseError],
    number_of_threads: usize,
) -> GrammarStore
{
    // Create scanner productions

    create_scanner_productions(&mut grammar, errors);

    // Update symbols

    finalize_symbols(&mut grammar, errors);

    // Get Item cache data.

    finalize_items(&mut grammar, number_of_threads, errors);

    // Get production meta data

    finalize_productions(&mut grammar, number_of_threads, errors);

    // Set bytecode ids

    finalize_byte_code_data(&mut grammar, errors);

    grammar
}

fn finalize_byte_code_data(
    grammar: &mut GrammarStore,
    errors: &mut [ParseError],
)
{
    let GrammarStore {
        production_table,
        bodies_table,
        ..
    } = grammar;

    for (index, body) in bodies_table
        .values_mut()
        .filter(|(b)| !production_table.get(&b.production).unwrap().is_scanner)
        .enumerate()
    {
        body.bytecode_id = index as u32;
    }

    for (index, production) in production_table.values_mut().enumerate() {
        production.bytecode_id = index as u32;
    }
}

fn finalize_productions(
    grammar: &mut GrammarStore,
    number_of_threads: usize,
    errors: &mut [ParseError],
)
{
    let production_ids =
        grammar.production_table.keys().cloned().collect::<Vec<_>>();

    let production_id_chunks = production_ids
        .chunks(number_of_threads)
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>();

    for (production_id, is_recursive, _) in thread::scope(|s| {
        production_id_chunks
            .iter()
            .map(|work| {
                s.spawn(|| {
                    work.iter()
                        .map(|production_id| {
                            // temp
                            let (is_recursive, is_left_recursive) =
                                is_production_recursive(
                                    *production_id,
                                    &*grammar,
                                );

                            (*production_id, is_recursive, is_left_recursive)
                        })
                        .collect::<Vec<_>>()
                })
            })
            // Collect now to actually generate the threads
            .collect::<Vec<_>>()
            .into_iter()
            .flat_map(move |s| s.join().unwrap())
            .collect::<Vec<_>>()
    }) {
        let production =
            grammar.production_table.get_mut(&production_id).unwrap();

        production.is_recursive = is_recursive;
    }
}

fn finalize_items(
    grammar: &mut GrammarStore,
    number_of_threads: usize,
    errors: &mut [ParseError],
)
{
    // Generate the item closure cache
    let start_items = grammar
        .production_table
        .keys()
        .flat_map(|p| get_production_start_items(p, &*grammar))
        .collect::<Vec<_>>();

    let start_items_chunks =
        start_items.chunks(number_of_threads).collect::<Vec<_>>();

    for (item, closure, peek_symbols) in thread::scope(|s| {
        start_items_chunks
            .iter()
            .map(|work| {
                s.spawn(|| {
                    let mut results = vec![];

                    let mut pending_items =
                        VecDeque::<Item>::from_iter(work.iter().cloned());

                    while let Some(item) = pending_items.pop_front() {
                        if !item.at_end() {
                            let peek_symbols = if let Some(peek_symbols) =
                                grammar
                                    .production_peek_symbols
                                    .get(&item.get_production_id(grammar))
                            {
                                peek_symbols.clone()
                            } else {
                                vec![]
                            };

                            results.push((
                                item,
                                get_closure(&[item], &*grammar),
                                peek_symbols,
                            ));

                            pending_items.push_back(item.increment().unwrap());
                        }
                    }

                    results
                })
            })
            // Collect now to actually generate the threads
            .collect::<Vec<_>>()
            .into_iter()
            .flat_map(move |s| s.join().unwrap())
            .collect::<Vec<_>>()
    }) {
        grammar
            .item_peek_symbols
            .insert(item.to_zero_state(), peek_symbols);
        grammar.closures.insert(item.to_zero_state(), closure);
    }

    fn insert(
        goto_items: &mut BTreeMap<ProductionId, HashSet<Item>>,
        production_id: &ProductionId,
        item: Item,
    )
    {
        if !goto_items.contains_key(production_id) {
            goto_items.insert(*production_id, HashSet::<Item>::new());
        }

        goto_items.get_mut(production_id).unwrap().insert(item);
    }

    for closure in grammar.closures.values().cloned() {
        for item in closure {
            if item.is_nonterm(grammar) {
                let production_id = &item.get_production_id(grammar);

                insert(&mut grammar.lr_items, production_id, item);
            }
        }
    }
}

struct WorkVerifier
{
    complete: u32,
    pending:  u32,
    progress: u32,
}

impl WorkVerifier
{
    pub fn new(pending: u32) -> Self
    {
        WorkVerifier {
            complete: 0,
            pending,
            progress: 0,
        }
    }

    pub fn add_units_of_work(&mut self, units_of_work: u32)
    {
        self.pending += units_of_work;
    }

    pub fn start_one_unit_of_work(&mut self)
    {
        if self.pending > 0 {
            self.pending -= 1;

            self.progress += 1;
        } else {
            panic!("No pending work")
        }
    }

    pub fn complete_one_unit_of_work(&mut self)
    {
        if self.progress > 0 {
            self.progress -= 1;

            self.complete += 1;
        } else {
            panic!("No work in progress")
        }
    }

    pub fn skip_one_unit_of_work(&mut self)
    {
        if self.pending > 0 {
            self.pending -= 1;

            self.complete += 1;
        } else {
            panic!("No pending work")
        }
    }

    pub fn is_complete(&self) -> bool
    {
        self.pending == 0 || self.progress == 0 || self.complete > 0
    }
}

fn finalize_symbols(grammar: &mut GrammarStore, errors: &mut [ParseError])
{
    let mut symbol_bytecode_id = SymbolID::DefinedSymbolIndexBasis;

    for sym_id in &SymbolID::Generics {
        let (_, production_id, ..) =
            get_scanner_info_from_defined(sym_id, grammar);

        let scanner_id = sym_id.bytecode_id(grammar);

        grammar
            .production_table
            .get_mut(&production_id)
            .unwrap()
            .symbol_bytecode_id = scanner_id;
    }

    for sym_id in grammar.symbols_table.keys().cloned().collect::<Vec<_>>() {
        if !grammar.symbols_table.get(&sym_id).unwrap().scanner_only {
            match sym_id {
                SymbolID::TokenProduction(..)
                | SymbolID::DefinedSymbol(_)
                | SymbolID::DefinedNumeric(_)
                | SymbolID::DefinedIdentifier(_) => {
                    let symbol =
                        grammar.symbols_table.get_mut(&sym_id).unwrap();

                    symbol.bytecode_id = symbol_bytecode_id;

                    let (_, production_id, ..) =
                        get_scanner_info_from_defined(&sym_id, &*grammar);

                    let scanner_production = grammar
                        .production_table
                        .get_mut(&production_id)
                        .unwrap();

                    scanner_production.symbol_bytecode_id = symbol_bytecode_id;

                    symbol_bytecode_id += 1;
                }
                _ => {}
            }
        }

        grammar
            .symbols_table
            .get_mut(&sym_id)
            .unwrap()
            .friendly_name = sym_id.to_string(grammar);
    }
}

fn create_scanner_productions(
    grammar: &mut GrammarStore,
    errors: &mut [ParseError],
)
{
    // Start iterating over known token production references, and add
    // new productions as we encounter them.
    let mut scanner_production_queue = VecDeque::from_iter(
        grammar
            .symbols_table
            .keys()
            .chain(SymbolID::Generics.iter())
            .cloned(),
    );

    while let Some(sym_id) = scanner_production_queue.pop_front() {
        match &sym_id {
            sym if SymbolID::Generics.contains(sym) => {
                let (_, scanner_production_id, scanner_name, symbol_string) =
                    get_scanner_info_from_defined(&sym_id, &*grammar);

                if let btree_map::Entry::Vacant(e) =
                    grammar.production_table.entry(scanner_production_id)
                {
                    // Insert into grammar any new defined symbol derived from
                    // token productions.

                    let body_id = BodyId::new(&scanner_production_id, 0);

                    grammar
                        .production_bodies_table
                        .insert(scanner_production_id, vec![body_id]);

                    grammar.bodies_table.insert(body_id, types::Body {
                        length: 1,
                        symbols: vec![BodySymbolRef {
                            annotation: String::default(),
                            consumable: true,
                            exclusive: false,
                            original_index: 0,
                            scanner_index: 0,
                            scanner_length: 1,
                            sym_id,
                            tok: Token::default(),
                        }],
                        production: scanner_production_id,
                        id: body_id,
                        bytecode_id: 0,
                        reduce_fn_ids: vec![],
                        origin_location: Token::empty(),
                    });

                    e.insert(crate::types::Production::new(
                        scanner_name,
                        scanner_production_id,
                        1,
                        Token::empty(),
                        true,
                    ));
                }
            }
            SymbolID::DefinedSymbol(_)
            | SymbolID::DefinedNumeric(_)
            | SymbolID::DefinedIdentifier(_) => {
                let (_, scanner_production_id, scanner_name, symbol_string) =
                    get_scanner_info_from_defined(&sym_id, &*grammar);

                let GrammarStore {
                    symbols_string_table,
                    symbols_table,
                    production_table,
                    production_bodies_table,
                    bodies_table,
                    ..
                } = grammar;

                if let btree_map::Entry::Vacant(e) =
                    production_table.entry(scanner_production_id)
                {
                    // Defined symbols are split along code points and
                    // individually packaged
                    let chars: Vec<char> = symbol_string.chars().collect();

                    let new_body_symbols: Vec<BodySymbolRef> = chars
                        .iter()
                        .enumerate()
                        .map(|(index, byte)| {
                            let string = byte.to_string();

                            let id = get_literal_id(&string);

                            symbols_table.entry(id).or_insert_with(|| {
                                symbols_string_table.insert(id, string);

                                Symbol {
                                    byte_length: byte.len_utf8() as u32,
                                    code_point_length: 1,
                                    bytecode_id: 0,
                                    uuid: id,
                                    scanner_only: true,
                                    friendly_name: id.to_default_string(),
                                }
                            });

                            BodySymbolRef {
                                annotation: String::default(),
                                consumable: true,
                                exclusive: false,
                                original_index: 0,
                                scanner_index: index as u32,
                                scanner_length: chars.len() as u32,
                                sym_id: id,
                                tok: Token::default(),
                            }
                        })
                        .collect();

                    // Insert into grammar any new defined symbol derived from
                    // token productions.

                    let body_id = BodyId::new(&scanner_production_id, 0);

                    production_bodies_table
                        .insert(scanner_production_id, vec![body_id]);

                    bodies_table.insert(body_id, Body {
                        length: new_body_symbols.len() as u16,
                        symbols: new_body_symbols,
                        production: scanner_production_id,
                        id: body_id,
                        bytecode_id: 0,
                        reduce_fn_ids: vec![],
                        origin_location: Token::empty(),
                    });

                    e.insert(crate::types::Production::new(
                        scanner_name,
                        scanner_production_id,
                        1,
                        Token::empty(),
                        true,
                    ));
                }
            }
            // This initially process token-production symbols dumped in to the
            // queue, but as we process these productions, we'll inevitably
            // encounter regular production symbols. The process of converting a
            // production symbol into scanner production is identical to that of
            // processing a token-production, so we can just combine the match
            // selectors of TokenProductions and Production
            // symbols.
            SymbolID::Production(prod_id, _)
            | SymbolID::TokenProduction(prod_id, _) => {
                let production =
                    grammar.production_table.get(prod_id).unwrap().clone();
                let scanner_name = create_scanner_name(&production.name);
                let scanner_production_id = ProductionId::from(&scanner_name);

                if !grammar
                    .production_table
                    .contains_key(&scanner_production_id)
                {
                    let scanner_bodies: Vec<Body> = grammar
                        .production_bodies_table
                        .get(prod_id)
                        .unwrap()
                        .iter()
                        .enumerate()
                        .map(|(body_index, body_id)| {
                            let natural_body =
                                grammar.bodies_table.get(body_id).unwrap();

                            let scanner_symbols =
                                natural_body.symbols.iter().flat_map(|sym| {
                                    let sym_id = &sym.sym_id;
                                    match sym_id {
                                        // For any production or token
                                        // production symbol encountered, create
                                        // a new symbol that references the
                                        // equivalent scanner production name,
                                        // and submit this production for
                                        // processing into a new scanner
                                        // production.
                                        SymbolID::Production(
                                            prod_id,
                                            grammar_id,
                                        )
                                        | SymbolID::TokenProduction(
                                            prod_id,
                                            grammar_id,
                                        ) => {
                                            let production = grammar
                                                .production_table
                                                .get(prod_id)
                                                .unwrap();

                                            let scanner_name =
                                                create_scanner_name(
                                                    &production.name,
                                                );

                                            let scanner_production_id =
                                                ProductionId::from(
                                                    &scanner_name,
                                                );

                                            let new_symbol_id =
                                                SymbolID::Production(
                                                    scanner_production_id,
                                                    *grammar_id,
                                                );

                                            scanner_production_queue
                                                .push_back(*sym_id);

                                            vec![BodySymbolRef {
                                                annotation: String::default(),
                                                consumable: true,
                                                exclusive: sym.exclusive,
                                                original_index: 0,
                                                scanner_index: 0,
                                                scanner_length: 1,
                                                sym_id: new_symbol_id,
                                                tok: Token::default(),
                                            }]
                                        }
                                        SymbolID::DefinedSymbol(_)
                                        | SymbolID::DefinedNumeric(_)
                                        | SymbolID::DefinedIdentifier(_) => {
                                            let (new_symbol_id, ..) =
                                                get_scanner_info_from_defined(
                                                    sym_id, &*grammar,
                                                );

                                            scanner_production_queue
                                                .push_back(*sym_id);

                                            vec![BodySymbolRef {
                                                annotation: String::default(),
                                                consumable: true,
                                                exclusive: sym.exclusive,
                                                original_index: 0,
                                                scanner_index: 0,
                                                scanner_length: 1,
                                                sym_id: new_symbol_id,
                                                tok: Token::default(),
                                            }]
                                        }
                                        _ => vec![sym.clone()],
                                    }
                                });

                            let symbols: Vec<BodySymbolRef> =
                                scanner_symbols.collect();

                            Body {
                                id: BodyId::new(
                                    &scanner_production_id,
                                    body_index,
                                ),
                                length: symbols.len() as u16,
                                production: scanner_production_id,
                                symbols,
                                bytecode_id: 0,
                                reduce_fn_ids: vec![],
                                origin_location: production
                                    .original_location
                                    .clone(),
                            }
                        })
                        .collect();

                    let mut bodies = vec![];

                    for body in scanner_bodies {
                        bodies.push(body.id);
                        grammar.bodies_table.insert(body.id, body);
                    }

                    grammar.production_table.insert(
                        scanner_production_id,
                        crate::types::Production::new(
                            scanner_name,
                            scanner_production_id,
                            bodies.len() as u16,
                            production.original_location.clone(),
                            true,
                        ),
                    );

                    grammar
                        .production_bodies_table
                        .insert(scanner_production_id, bodies);
                }
            }
            _ => {}
        }
    }
}

#[inline]
pub(crate) fn get_scanner_info_from_defined(
    sym_id: &SymbolID,
    root: &GrammarStore,
) -> (SymbolID, ProductionId, String, String)
{
    let (scanner_name, symbol_string) = match sym_id {
        SymbolID::DefinedIdentifier(..)
        | SymbolID::DefinedNumeric(..)
        | SymbolID::DefinedSymbol(..) => {
            let symbol_string =
                root.symbols_string_table.get(sym_id).unwrap().clone();
            (create_defined_scanner_name(&symbol_string), symbol_string)
        }
        SymbolID::TokenProduction(production_id, _) => {
            let symbol_string = root
                .production_table
                .get(production_id)
                .unwrap()
                .name
                .clone();
            (create_scanner_name(&symbol_string), symbol_string)
        }
        sym => {
            let symbol_string = sym.to_default_string();
            (create_scanner_name(&symbol_string), symbol_string)
        }
    };

    let scanner_production_id = ProductionId::from(&scanner_name);

    let new_symbol_id = SymbolID::Production(scanner_production_id, root.guid);

    (new_symbol_id, scanner_production_id, scanner_name, symbol_string)
}

/// Merge related grammars into a single GrammarStore
///
/// `root` is assumed to derived from the root source grammar, and
/// grammars are all other GrammarStores derived from grammars
/// imported directly or indirectly from the root source grammar.

fn merge_grammars(
    root: &mut GrammarStore,
    grammars: &[GrammarStore],
    errors: &mut Vec<ParseError>,
)
{
    let mut grammars_lookup = HashMap::<GrammarId, &GrammarStore>::new();

    // Merge grammar data into a single store
    for import_grammar in grammars {
        grammars_lookup.insert(import_grammar.guid, import_grammar);

        root.production_peek_symbols
            .extend(import_grammar.production_peek_symbols.clone().into_iter());

        // Merge all symbols
        for (id, sym) in &import_grammar.symbols_table {
            if !root.symbols_table.contains_key(id) {
                root.symbols_table.insert(*id, sym.clone());

                match id {
                    SymbolID::DefinedSymbol(_)
                    | SymbolID::DefinedNumeric(_)
                    | SymbolID::DefinedIdentifier(_) => {
                        match import_grammar.symbols_string_table.get(id) {
                            Some(string) => {
                                root.symbols_string_table
                                    .insert(*id, string.to_owned());
                            }
                            None => {}
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    // Merge all referenced foreign productions into the root.
    let mut symbol_queue =
        VecDeque::from_iter(root.production_symbols_table.clone());

    while let Some((sym, tok)) = symbol_queue.pop_front() {
        if let Some(grammar_id) = sym.getGrammarId() {
            if grammar_id != root.guid {
                match grammars_lookup.get(&grammar_id) {
                    Some(import_grammar) => {
                        if let Some(prod_id) = sym.getProductionId() {
                            if let std::collections::btree_map::Entry::Vacant(
                                e,
                            ) = root.production_table.entry(prod_id)
                            {
                                match import_grammar
                                    .production_table
                                    .get(&prod_id)
                                {
                                    Some(production) => {
                                        // import the foreign production
                                        e.insert(production.clone());

                                        let bodies = import_grammar
                                            .production_bodies_table
                                            .get(&prod_id)
                                            .unwrap()
                                            .clone();

                                        // Import all bodies referenced by this
                                        // production
                                        for body_id in &bodies {
                                            let body = import_grammar
                                                .bodies_table
                                                .get(body_id)
                                                .unwrap()
                                                .clone();

                                            // Add every Production symbol to
                                            // the queue
                                            for sym in &body.symbols {
                                                match sym.sym_id {
                                                    SymbolID::Production(..) => {
                                                        symbol_queue
                                                            .push_back((sym.sym_id, sym.tok.clone()))
                                                    }
                                                    SymbolID::TokenProduction(
                                                        prod,
                                                        grammar,
                                                    ) => {
                                                        root
                                                            .symbols_table.entry(sym.sym_id).or_insert_with(
                                                                || (import_grammar
                                                                    .symbols_table
                                                                    .get(&sym.sym_id)
                                                                    .unwrap()
                                                                    .clone())
                                                            );

                                                        // Remap the production token symbol to regular a production symbol and submit as a merge candidate.
                                                        symbol_queue.push_back((
                                                            SymbolID::Production(
                                                                prod, grammar,
                                                            ),
                                                            sym.tok.clone()
                                                        ))
                                                    }
                                                    _ => {}
                                                }
                                            }

                                            root.bodies_table
                                                .insert(*body_id, body);
                                        }

                                        // Import the map of production id to
                                        // bodies
                                        root.production_bodies_table
                                            .insert(prod_id, bodies);
                                    }
                                    None => {
                                        errors.push(ParseError::COMPILE_PROBLEM(
                                            CompileProblem {
                                                message:        format!(
                                                    "Can't find production {}::{} in {:?} \n{}",
                                                    get_production_plain_name(&prod_id, root), grammar_id, import_grammar.source_path,
                                                    tok.blame(1, 1, "").unwrap()
                                                ),
                                                inline_message: String::new(),
                                                loc:            Token::empty(),
                                            },
                                        ));
                                    }
                                }
                            }
                        }
                    }
                    None => {}
                }
            }
        }
    }
}

/// Takes a Grammar produces core primitive tables;
///
/// ## Arguments
///
/// - `grammar` - A hcg AST node
/// - `source` - The source string of the hcg.
/// - `absolute_path` - The absolute path of the hcg's source file
///   Used to resolve linked grammars.
///  

pub fn pre_process_grammar(
    grammar: &ast::Grammar,
    absolute_path: &PathBuf,
) -> (GrammarStore, Vec<ParseError>)
{
    let mut import_names_lookup = ImportProductionNameTable::new();

    let mut production_bodies_table = ProductionBodiesTable::new();

    let mut production_table = ProductionTable::new();

    let mut bodies_table = BodyTable::new();

    let mut symbols_table = SymbolsTable::new();

    let mut symbols_string_table = SymbolStringTable::new();

    let mut post_process_productions: VecDeque<Box<ast::Production>> =
        VecDeque::new();

    let mut production_symbols_table = BTreeMap::new();

    let uuid_name = get_uuid_grammar_name(absolute_path).unwrap();

    let uuid = GrammarId(hash_id_value_u64(&uuid_name));

    let mut reduce_functions = ReduceFunctionTable::new();

    let mut parse_errors = vec![];

    let mut global_peek_symbols = vec![];

    let mut export_names = vec![];

    {
        let mut tgs = TempGrammarStore {
            local_guid: &uuid_name,
            absolute_path,
            import_names_lookup: &mut import_names_lookup,
            symbols_table: &mut symbols_table,
            symbols_string_table: &mut symbols_string_table,
            bodies_table: &mut bodies_table,
            production_table: &mut production_table,
            production_symbols_table: &mut production_symbols_table,
            production_bodies_table: &mut production_bodies_table,
            errors: &mut parse_errors,
            reduce_functions: &mut reduce_functions,
        };

        // Process meta data, including EXPORT, IMPORT, and IGNORE meta
        // data
        for obj in grammar.preamble.iter() {
            match obj {
                ASTNode::Ignore(box ast::Ignore { symbols }) => {
                    for symbol in symbols {
                        if let Some(id) = intern_symbol(symbol, &mut tgs) {
                            global_peek_symbols.push(id)
                        }
                    }
                }
                ASTNode::Import(import) => {
                    let mut uri = PathBuf::from(&import.uri);

                    let local_name = import.reference.to_string();

                    // Resolve path names. Since this touches the filesystem,
                    // it's bypassed when running tests to keep tests pure.

                    if !uri.is_absolute() {
                        match absolute_path.parent() {
                            None => {}
                            Some(new_path) => {
                                let mut new_path = new_path.to_owned();

                                new_path.push(uri);

                                match new_path.canonicalize() {
                                    Ok(result) => {
                                        uri = result;
                                    }
                                    Err(err) => {
                                        tgs.errors.push(ParseError::COMPILE_PROBLEM(
                                            CompileProblem {
                                                message:
                                                    format!("Problem encountered when verifying imported grammar [{}]", local_name),
                                                inline_message: "Could not find imported grammar".to_string(),
                                                loc: import.Token(),
                                            },
                                        ));
                                        continue;
                                    }
                                }
                            }
                        }
                    }

                    let import_uuid = get_uuid_grammar_name(&uri).unwrap();

                    // Map the foreign grammar's local name to the uuid and
                    // absolute path

                    tgs.import_names_lookup
                        .insert(local_name, (import_uuid, uri));
                }
                ASTNode::Export(box ast::Export {
                    production,
                    reference,
                }) => {
                    let production_id =
                        get_production_id_from_node(production, &mut tgs);
                    export_names.push((production_id, reference.to_string()));
                }
                _ => {}
            }
        }

        // Process main grammar data, which include
        // Productions, IR states, and out of band functions
        for node in grammar.content.iter() {
            match node {
                ASTNode::Production(_) => {
                    let production = pre_process_production(
                        node,
                        &mut tgs,
                        &mut post_process_productions,
                    );
                    if production != ProductionId::default()
                        && export_names.is_empty()
                    {
                        export_names.push((production, "default".to_string()));
                    }
                }
                ASTNode::ProductionMerged(prod) => {}
                ASTNode::IR_STATE(ir_state) => {}
                ASTNode::Out_Of_Band(oob_fn) => {}
                _ => {}
            }
        }

        // Continue processing any generated productions. This may loop
        // for a while as any given production may have several nested
        // anonymous productions through lists `...(+) | ...(*)` and
        // groups `(... | ...)`

        while let Some(node) = post_process_productions.pop_front() {
            pre_process_production(
                &ASTNode::Production(node),
                &mut tgs,
                &mut post_process_productions,
            );
        }
    }

    let production_peek_symbols = production_table
        .keys()
        .map(|k| (*k, global_peek_symbols.clone()))
        .collect::<HashMap<_, _>>();

    (
        GrammarStore {
            source_path: absolute_path.clone(),
            guid: uuid,
            guid_name: uuid_name,
            production_bodies_table,
            production_table,
            bodies_table,
            symbols_table,
            symbols_string_table,
            production_symbols_table,
            imports: import_names_lookup,
            closures: HashMap::new(),
            item_peek_symbols: HashMap::new(),
            production_peek_symbols,
            lr_items: BTreeMap::new(),
            reduce_functions,
            export_names,
        },
        parse_errors,
    )
}

fn pre_process_production(
    production_node: &ASTNode,
    tgs: &mut TempGrammarStore,
    post_process_productions: &mut VecDeque<Box<ast::Production>>,
) -> ProductionId
{
    let mut body_index = 0;

    if let ASTNode::Production(prod) = production_node {
        let production_id = get_production_id_from_node(production_node, tgs);
        let production_name =
            get_resolved_production_name(production_node, tgs).unwrap();
        let mut bodies = vec![];

        if match tgs.production_table.get(&production_id) {
            Some(existing_production) => {
                tgs.errors.push({
                    ParseError::COMPOUND_COMPILE_PROBLEM(
                        CompoundCompileProblem {
                            message:   format!(
                                "production {} already exists!",
                                production_name
                            ),
                            locations: vec![
                                CompileProblem {
                                    inline_message: String::new(),
                                    loc: production_node.Token(),
                                    message: format!(
                                        "Redefinition of {} occurs here.",
                                        production_name
                                    ),
                                },
                                CompileProblem {
                                    inline_message: String::new(),
                                    loc: existing_production
                                        .original_location
                                        .clone(),
                                    message: format!(
                                        "production {} first defined here.",
                                        production_name
                                    ),
                                },
                            ],
                        },
                    )
                });
                false
            }
            None => true,
        } {
            // Extract body data and gather symbol information
            for body in &prod.bodies {
                if let ASTNode::Body(body) = body {
                    let (new_bodies, productions) =
                        pre_process_body(production_node, body, tgs);

                    for prod in productions {
                        post_process_productions.push_back(prod);
                    }

                    for mut body in new_bodies {
                        let id = BodyId::new(&production_id, body_index);

                        body.id = id;

                        tgs.bodies_table.insert(id, body);

                        bodies.push(id);

                        body_index += 1;
                    }
                }
            }

            tgs.production_table.insert(
                production_id,
                crate::types::Production::new(
                    production_name,
                    production_id,
                    bodies.len() as u16,
                    production_node.Token(),
                    false,
                ),
            );

            tgs.production_bodies_table.insert(production_id, bodies);

            production_id
        } else {
            ProductionId::default()
        }
    } else {
        ProductionId::default()
    }
}

/// Get the resolved production of name applicable nodes.
/// Nodes from which a production name can be derived:
/// - Production_Symbol
/// - Production_Token
/// - Production
/// - Import_Production
///
/// ## Panics
/// This function panics if the node is not one of the above.
///
/// This function also panics if a local imported grammar name does
/// not have a matching `@IMPORT` statement.

fn get_resolved_production_name(
    node: &ASTNode,
    tgs: &mut TempGrammarStore,
) -> Option<String>
{
    match node {
        ASTNode::Production_Import_Symbol(prod_imp_sym) => {
            let production_name = &prod_imp_sym.name;

            let local_import_grammar_name = &prod_imp_sym.module;

            match tgs.import_names_lookup.get(local_import_grammar_name) {
                None => {
                    tgs.errors.push(ParseError::COMPILE_PROBLEM(CompileProblem{
                        inline_message: String::new(),
                        message: format!(
                            "Unknown Grammar : The local grammar name {} does not match any imported grammar names", 
                            local_import_grammar_name
                        ),
                        loc: node.Token(),
                    }));
                    None
                }
                Some((grammar_uuid_name, _)) => {
                    Some(create_production_uuid_name(
                        grammar_uuid_name,
                        production_name,
                    ))
                }
            }
        }
        ASTNode::Production_Symbol(prod_sym) => {
            Some(create_production_uuid_name(tgs.local_guid, &prod_sym.name))
        }
        ASTNode::Production(prod) => {
            get_resolved_production_name(&prod.symbol, tgs)
        }
        ASTNode::Production_Token(prod_tok) => {
            get_resolved_production_name(&prod_tok.production, tgs)
        }
        _ => {
            tgs.errors
                .push(ParseError::COMPILE_PROBLEM(CompileProblem {
                    inline_message: String::new(),
                    message:
                        "Unexpected node: Unable to resolve production name of this node!"
                            .to_string(),
                    loc:            node.Token(),
                }));
            None
        }
    }
}

/// Get the resolved grammar data of applicable nodes.
/// Nodes from which a grammar name can be derived:
/// - Production_Symbol
/// - Production_Token
/// - Production
/// - Import_Production
///
/// ## Returns
/// A Tuple comprised of the grammar 0:uuid_name, 1:local_name, and
/// 2:absolute_path. local_name is `root` if the grammar maps to
/// currently rendered grammar.
///
/// ## Panics
/// This function panics if the node is not one of the above.
///
/// This function also panics if a local imported grammar name does
/// not have a matching `@IMPORT` statement.

fn get_grammar_info_from_node<'a>(
    node: &'a ASTNode,
    tgs: &'a mut TempGrammarStore,
) -> Option<(&'a str, &'a str, &'a PathBuf)>
{
    match node {
        ASTNode::Production_Import_Symbol(prod_imp_sym) => {
            let production_name = &prod_imp_sym.name;

            let local_import_grammar_name = &prod_imp_sym.module;

            match tgs.import_names_lookup.get(local_import_grammar_name) {
                None => {
                    tgs.errors.push(ParseError::COMPILE_PROBLEM(CompileProblem{
                        inline_message: String::new(),
                        message: format!(
                            "Unknown grammar : The local grammar name {} does not match any imported grammar names", 
                            local_import_grammar_name
                        ),
                        loc: node.Token(),
                    }));
                    None
                }
                Some((resolved_grammar_name, path)) => Some((
                    resolved_grammar_name,
                    local_import_grammar_name,
                    path,
                )),
            }
        }
        ASTNode::Production_Symbol(prod_sym) => {
            Some((tgs.local_guid, "root", tgs.absolute_path))
        }
        ASTNode::Production(prod) => {
            get_grammar_info_from_node(&prod.symbol, tgs)
        }
        ASTNode::Production_Token(prod_tok) => {
            get_grammar_info_from_node(&prod_tok.production, tgs)
        }
        _ => {
            tgs.errors
                .push(ParseError::COMPILE_PROBLEM(CompileProblem {
                    inline_message: String::new(),
                    message:
                        "Unexpected node: Unable to resolve production name of this node!"
                            .to_string(),
                    loc:            node.Token(),
                }));

            None
        }
    }
}

fn get_production_id_from_node(
    production: &ASTNode,
    tgs: &mut TempGrammarStore,
) -> ProductionId
{
    if let Some(name) = get_resolved_production_name(production, tgs) {
        ProductionId(hash_id_value_u64(name))
    } else {
        ProductionId(0)
    }
}

fn pre_process_body(
    production: &ASTNode,
    body: &ast::Body,
    tgs: &mut TempGrammarStore,
) -> (Vec<types::Body>, Vec<Box<ast::Production>>)
{
    if let ASTNode::Returned(ret) = &body.reduce_function {
        // Extract the function and insert into function table?
    }

    let production_name =
        get_resolved_production_name(production, tgs).unwrap();

    fn create_body_vectors(
        token: &Token,
        symbols: &Vec<ASTNode>,
        production_name: &String,
        mut index: u32,
        tgs: &mut TempGrammarStore,
    ) -> (Vec<(Token, Vec<BodySymbolRef>)>, Vec<Box<ast::Production>>)
    {
        let mut bodies = vec![];

        let mut productions = vec![];

        bodies.push((token.clone(), vec![]));

        for sym in symbols {
            let starting_bodies = bodies.len();

            fn create_production(
                name: &str,
                bodies: &[ASTNode],
                token: Token,
            ) -> (ASTNode, Box<ast::Production>)
            {
                // Create a virtual production and symbol to go in its place
                let symbol = ASTNode::Production_Symbol(
                    super::data::ast::Production_Symbol::new(
                        name.to_string(),
                        token.clone(),
                    ),
                );

                let production = super::data::ast::Production::new(
                    false,
                    symbol.clone(),
                    bodies.to_vec(),
                    false,
                    token,
                );

                (symbol, production)
            }

            let SymbolData {
                annotation,
                is_list,
                is_group,
                is_optional,
                is_no_consume,
                is_meta,
                is_exclusive,
                sym_atom,
            } = get_symbol_details(sym, tgs);

            if let Some(mut sym) = sym_atom.to_owned() {
                let mut generated_symbol = ASTNode::NONE;

                if is_meta {
                    // TODO: Separate meta data symbols into it's own table that
                    // maps meta symbols to a body and its
                    // index.
                    continue;
                }

                if is_group {
                    // Need to create new production that the virtual group
                    // production is bound to, add it to the list of
                    // currently considered productions, and replace
                    // this symbol with a production symbol pointing
                    // to the group.

                    // Except, if there are no functions within the production
                    // bodies we can simply inline the symbols into
                    // one or more alternate sets of bodies alongside
                    // the existing bodies.

                    if let ASTNode::Group_Production(group) = sym {
                        if annotation.is_empty()
                            && group.bodies.iter().all(|b| {
                                if let ASTNode::Body(body) = b {
                                    // The body does not have a reduce function.
                                    body.reduce_function.GetType() == 0
                                } else {
                                    false
                                }
                            })
                        {
                            // For each body in the group clone the existing
                            // body
                            // lists and process each list
                            // independently, inserting the new symbols
                            // into the existing bodies. We must make sure the
                            // indices are preserved since only the
                            // last symbol in each body can be bound
                            // to the index of the group production
                            // symbol.

                            let mut pending_bodies = vec![];

                            for body in &group.bodies {
                                if let ASTNode::Body(body) = body {
                                    let (mut new_bodies, mut new_productions) =
                                        create_body_vectors(
                                            &sym.Token(),
                                            &body.symbols,
                                            production_name,
                                            9999,
                                            tgs,
                                        );

                                    for (_, body) in new_bodies.iter_mut() {
                                        if let Some((last)) = body.last_mut() {
                                            last.original_index = index;
                                        }
                                    }

                                    pending_bodies.append(&mut new_bodies);

                                    productions.append(&mut new_productions);
                                }
                            }

                            let mut new_bodies = vec![];

                            for pending_body in pending_bodies {
                                for body in &mut bodies {
                                    let mut new_body = body.clone();

                                    new_body
                                        .1
                                        .extend(pending_body.1.iter().cloned());

                                    new_bodies.push(new_body)
                                }
                            }

                            bodies = new_bodies;

                            index += 1;

                            // We do not to process the existing symbol as it is
                            // now replaced with
                            // it's component symbols,
                            // so we'll skip the rest of the loop
                            continue;
                        } else {
                            let (prod_sym, production) = create_production(
                                &(production_name.to_owned()
                                    + "_group_"
                                    + &index.to_string()),
                                &group.bodies,
                                group.tok.clone(),
                            );

                            productions.push(production);

                            generated_symbol = prod_sym;

                            sym = &generated_symbol;
                        }
                    } else {
                        tgs.errors.push(ParseError::COMPILE_PROBLEM(
                            CompileProblem {
                                inline_message: String::new(),
                                message: "I don't know what to do with this."
                                    .to_string(),
                                loc: sym.Token(),
                            },
                        ));
                    }
                }

                if is_list {
                    // Create a new production that turns `A => a` into
                    // `A => a | A => A a` and produce a symbol id that points
                    // to that production.
                    static none_: ASTNode = ASTNode::NONE;
                    match sym {
                        ASTNode::Optional_List_Production(_)
                        | ASTNode::List_Production(_) => {
                            let (symbols, terminal_symbol, tok) = match sym {
                                ASTNode::Optional_List_Production(list) => (
                                    &list.symbols,
                                    &list.terminal_symbol,
                                    list.tok.clone(),
                                ),
                                ASTNode::List_Production(list) => (
                                    &list.symbols,
                                    &list.terminal_symbol,
                                    list.tok.clone(),
                                ),
                                _ => (&none_, &none_, Token::new()),
                            };

                            // Create new bodies that will be bound to the
                            // symbol.
                            let body_a = super::data::ast::Body::new(
                                false,
                                vec![symbols.clone()],
                                None,
                                ASTNode::NONE,
                                sym.Token(),
                            );

                            let mut body_b = body_a.clone();

                            match terminal_symbol {
                                ASTNode::NONE => {}
                                _ => {
                                    body_b
                                        .symbols
                                        .insert(0, terminal_symbol.clone());
                                }
                            }

                            let (prod_sym, mut production) = create_production(
                                &(production_name.to_owned()
                                    + "_list_"
                                    + &index.to_string()),
                                &[ASTNode::Body(body_b), ASTNode::Body(body_a)],
                                tok.clone(),
                            );

                            // Add the production symbol to the front of body be
                            // to make the body left
                            // recursive
                            if let ASTNode::Body(body) =
                                &mut production.bodies[0]
                            {
                                body.symbols.insert(0, prod_sym.clone());
                            }

                            productions.push(production);

                            generated_symbol = prod_sym;

                            sym = &generated_symbol;
                        }
                        _ => {
                            tgs.errors.push(ParseError::COMPILE_PROBLEM(
                                CompileProblem {
                                    inline_message: String::new(),
                                    message:
                                        "I don't know what to do with this."
                                            .to_string(),
                                    loc: sym.Token(),
                                },
                            ));
                        }
                    }
                }

                if is_optional {
                    // Need to create new bodies that contains all permutations
                    // of encountered symbols except for the currently
                    // considered symbol. This is achieved by duplicating all
                    // body vectors, then adding the current symbol to the
                    // original vectors, but not the duplicates.
                    for entry in bodies.clone() {
                        bodies.push(entry)
                    }
                }

                if let Some(id) = intern_symbol(sym, tgs) {
                    for (_, vec) in &mut bodies[0..starting_bodies] {
                        vec.push(BodySymbolRef {
                            original_index: index,
                            sym_id: id,
                            annotation: annotation.clone(),
                            consumable: !is_no_consume,
                            exclusive: is_exclusive,
                            scanner_index: 0,
                            scanner_length: 0,
                            tok: sym.Token(),
                        });
                    }
                }

                index += 1;
            }
        }

        (bodies, productions)
    }

    let (bodies, productions) = create_body_vectors(
        &body.Token(),
        &body.symbols,
        &production_name,
        0,
        tgs,
    );

    let reduce_fn_ids = match body.reduce_function {
        ASTNode::Reduce(..) | ASTNode::Ascript(..) => {
            let reduce_id = ReduceFunctionId::new(&body.reduce_function);

            tgs.reduce_functions.entry(reduce_id).or_insert_with(|| {
                ReduceFunctionType::new(&body.reduce_function)
            });

            vec![reduce_id]
        }
        _ => vec![],
    };

    (
        bodies
            .iter()
            .map(|(t, b)| types::Body {
                symbols: b.clone(),
                length: b.len() as u16,
                production: get_production_id_from_node(production, tgs),
                id: BodyId::default(),
                bytecode_id: 0,
                reduce_fn_ids: reduce_fn_ids.clone(),
                origin_location: t.clone(),
            })
            .collect(),
        productions,
    )
}

/// Returns an appropriate SymbolID::Defined* based on the input
/// string

fn get_literal_id(string: &String) -> SymbolID
{
    let identifier = Regex::new(r"[\w_-][\w\d_-]*$").unwrap();

    let number = Regex::new(r"\d+$").unwrap();

    if number.is_match(string) {
        SymbolID::DefinedNumeric(StringId::from(string))
    } else if identifier.is_match(string) {
        SymbolID::DefinedIdentifier(StringId::from(string))
    } else {
        SymbolID::DefinedSymbol(StringId::from(string))
    }
}

/// Adds a symbol to the symbol_table

fn intern_symbol(
    sym: &ASTNode, // , symbols_table,
    tgs: &mut TempGrammarStore,
) -> Option<SymbolID>
{
    fn process_literal(string: &String, tgs: &mut TempGrammarStore)
        -> SymbolID
    {
        let mut id = get_literal_id(string);

        if let std::collections::btree_map::Entry::Vacant(e) =
            tgs.symbols_table.entry(id)
        {
            tgs.symbols_string_table.insert(id, string.to_owned());

            let byte_length = string.bytes().len() as u32;

            let code_point_length = string.chars().count() as u32;

            e.insert(Symbol {
                bytecode_id: 0,
                uuid: id,
                byte_length,
                code_point_length,
                scanner_only: false,
                friendly_name: String::new(),
            });
        }

        id
    }

    fn get_production_hash_ids(
        node: &ASTNode,
        tgs: &mut TempGrammarStore,
    ) -> Option<(ProductionId, GrammarId)>
    {
        match node {
            ASTNode::Production_Symbol(_)
            | ASTNode::Production_Import_Symbol(_) => {
                let production_id = get_production_id_from_node(node, tgs);
                get_grammar_info_from_node(node, tgs).map(|data| {
                    (production_id, GrammarId(hash_id_value_u64(data.0)))
                })
            }
            _ => {
                tgs.errors.push(ParseError::COMPILE_PROBLEM(CompileProblem {
                    inline_message: "This is not a hashable production symbol."
                        .to_string(),
                    message: "[INTERNAL ERROR]".to_string(),
                    loc: node.Token(),
                }));
                None
            }
        }
    }

    fn process_production(
        node: &ASTNode,
        tgs: &mut TempGrammarStore,
        tok: Token,
    ) -> Option<SymbolID>
    {
        if let Some((production_id, grammar_id)) =
            get_production_hash_ids(node, tgs)
        {
            let id = SymbolID::Production(production_id, grammar_id);

            tgs.production_symbols_table.insert(id, tok);

            Some(id)
        } else {
            None
        }
    }

    fn process_token_production(
        node: &ast::Production_Token,
        tgs: &mut TempGrammarStore,
        tok: Token,
    ) -> Option<SymbolID>
    {
        match process_production(&node.production, tgs, tok) {
            Some(SymbolID::Production(prod_id, grammar_id)) => {
                let production_id = SymbolID::Production(prod_id, grammar_id);

                let token_production_id =
                    SymbolID::TokenProduction(prod_id, grammar_id);

                tgs.symbols_table.entry(token_production_id).or_insert(
                    Symbol {
                        bytecode_id: 0,
                        uuid: production_id,
                        byte_length: 0,
                        code_point_length: 0,
                        scanner_only: false,
                        friendly_name: String::new(),
                    },
                );

                Some(token_production_id)
            }
            _ => None,
        }
    }

    match sym {
        ASTNode::Generated(gen) => match gen.val.as_str() {
            "sp" => Some(SymbolID::GenericSpace),
            "tab" => Some(SymbolID::GenericHorizontalTab),
            "nl" => Some(SymbolID::GenericNewLine),
            "id" => Some(SymbolID::GenericIdentifier),
            "num" => Some(SymbolID::GenericNumber),
            "sym" => Some(SymbolID::GenericSymbol),
            _ => Some(SymbolID::Undefined),
        },
        ASTNode::Exclusive_Literal(literal) => {
            Some(process_literal(&literal.val, tgs))
        }
        ASTNode::Literal(literal) => Some(process_literal(&literal.val, tgs)),
        ASTNode::End_Of_File(_) => Some(SymbolID::EndOfFile),
        ASTNode::Production_Symbol(_)
        | ASTNode::Production_Import_Symbol(_) => {
            process_production(sym, tgs, sym.Token())
        }
        ASTNode::Production_Token(token) => {
            process_token_production(token, tgs, sym.Token())
        }
        _ => {
            tgs.errors.push(ParseError::COMPILE_PROBLEM(CompileProblem {
                inline_message:
                    "Unexpected ASTNode while attempting to intern symbol"
                        .to_string(),
                message: "[INTERNAL ERROR]".to_string(),
                loc: sym.Token(),
            }));
            None
        }
    }
}

struct SymbolData<'a>
{
    pub annotation:    String,
    pub is_list:       bool,
    pub is_group:      bool,
    pub is_optional:   bool,
    pub is_no_consume: bool,
    pub is_meta:       bool,
    pub is_exclusive:  bool,
    pub sym_atom:      Option<&'a ASTNode>,
}

/// Get a flattened view of a symbol's immediate AST

fn get_symbol_details<'a>(
    mut sym: &'a ASTNode,
    tgs: &mut TempGrammarStore,
) -> SymbolData<'a>
{
    let mut data = SymbolData {
        annotation:    String::new(),
        is_list:       false,
        is_group:      false,
        is_optional:   false,
        is_no_consume: false,
        is_meta:       false,
        is_exclusive:  false,
        sym_atom:      None,
    };

    loop {
        match sym {
            ASTNode::AnnotatedSymbol(annotated) => {
                // Removes the dangling `^`, as in `^annotation_name`
                data.annotation = annotated.reference.val[1..].to_owned();
                sym = &annotated.symbol;
            }
            ASTNode::OptionalSymbol(optional) => {
                data.is_optional = true;
                sym = &optional.symbol;
            }
            ASTNode::NonCaptureSymbol(non_cap) => {
                data.is_no_consume = true;
                sym = &non_cap.sym;
            }
            ASTNode::Exclude(_) | ASTNode::Look_Ignore(_) => {
                data.is_meta = true;
                break;
            }
            ASTNode::Group_Production(_) => {
                data.is_group = true;
                break;
            }
            ASTNode::List_Production(_) => {
                data.is_list = true;
                break;
            }
            ASTNode::Optional_List_Production(_) => {
                data.is_list = true;
                data.is_optional = true;
                break;
            }
            ASTNode::Exclusive_Literal(_) => {
                data.is_exclusive = true;
                break;
            }
            // This symbol types are "real" symbols, in as much
            // as they represent actual parsable entities which are
            // submitted to the bytecode compiler for evaluation
            ASTNode::Generated(_)
            | ASTNode::Literal(_)
            | ASTNode::Empty(_)
            | ASTNode::End_Of_File(_)
            | ASTNode::Production_Symbol(_)
            | ASTNode::Production_Token(_)
            | ASTNode::Production_Import_Symbol(_) => {
                break;
            }
            _ => {
                tgs.errors.push(ParseError::COMPILE_PROBLEM(CompileProblem {
                    inline_message: format!(
                        "Unexpected ASTNode {}",
                        sym.GetType()
                    ),
                    message: "[INTERNAL ERROR]".to_string(),
                    loc: sym.Token(),
                }));
                break;
            }
        }
    }

    data.sym_atom = Some(sym);

    data
}
