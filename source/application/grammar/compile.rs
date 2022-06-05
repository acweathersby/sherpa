use hctk::primitives::{
    production, Body, BodySymbolRef, BodyTable, GrammarStore, HCGSource, ProductionBodiesTable,
    ProductionId, ProductionTable, Symbol, SymbolID, SymbolStringTable, SymbolsTable, Token,
};
use regex::Regex;

use crate::{grammar_data::ast::Body as ASTBody, grammar_data::ast::*, parse};

/// Takes a Grammar produces core primitive tables;
///
/// ## Arguments
///
/// - `grammar` - A grammar AST node
/// - `origin` - The absolute path of the grammar's source file Used to resolve linked grammars.
///  
fn pre_process_grammar(grammar: &Grammar, source: &String, origin: &String) -> GrammarStore {
    // Process meta data, including EXPORT, IMPORT,
    // IGNORE meta data
    for obj in grammar.preamble.iter() {
        match obj {
            ASTNode::Ignore(ignore) => {}
            ASTNode::Import(import) => {
                let uri = &import.uri;
                let name = import.reference.String();

                println!("importing {} from {}", name, uri);
            }
            ASTNode::Export(export) => {}
            _ => {}
        }
    }

    let mut production_index = 0;
    let mut body_index = 0;
    let mut production_bodies_table = ProductionBodiesTable::new();
    let mut production_table = ProductionTable::new();
    let mut bodies_table = BodyTable::new();
    let mut symbols_table = SymbolsTable::new();
    let mut symbols_string_table = SymbolStringTable::new();
    let mut post_process_productions: VecDeque<Box<Production>> = VecDeque::new();

    // Process main grammar data, which include
    // Productions, IR states, and out of band functions
    for obj in grammar.content.iter() {
        match obj {
            ASTNode::Production(prod) => {
                let production_id = {
                    production_index += 1;
                    production_index as ProductionId
                };

                let production_name = {
                    match &prod.symbol {
                        ASTNode::Production_Symbol(sym) => sym.name.to_owned(),
                        ASTNode::Production_Import_Symbol(imp_sym) => imp_sym.name.to_owned(),
                        _ => String::from(""),
                    }
                };

                let mut bodies: Vec<u16> = vec![];

                // Extract body data and gather symbol information
                for body in &prod.bodies {
                    if let ASTNode::Body(body) = body {
                        let (new_bodies, productions) = pre_process_body(
                            production_id,
                            body,
                            &mut symbols_table,
                            &mut symbols_string_table,
                        );

                        for prod in productions {
                            post_process_productions.push_back(prod);
                        }

                        for mut body in new_bodies {
                            body.id = body_index;
                            bodies_table.insert(body_index, body);
                            bodies.push(body_index);
                            body_index += 1;
                        }
                    }
                }

                production_table.insert(
                    production_id,
                    hctk::primitives::Production {
                        id: production_id,
                        name: production_name,
                        number_of_bodies: bodies.len() as u16,
                        is_scanner: false,
                        is_entry: false,
                        is_recursive: false,
                        priority: 0,
                    },
                );

                production_bodies_table.insert(production_id, (production_id, bodies));
            }
            ASTNode::ProductionMerged(prod) => {}
            ASTNode::IR_STATE(prod) => {}
            ASTNode::Out_Of_Band(oob_fn) => {}
            _ => {}
        }
    }

    println!(
        "{:#?}, {:#?}, {:#?}",
        bodies_table, symbols_table, symbols_string_table
    );

    GrammarStore {
        sources: vec![HCGSource {
            absolute_path: origin.to_owned(),
            source: source.to_owned(),
        }],
        production_bodies_table,
        production_table,
        bodies_table,
        symbols_table,
        symbols_string_table,
    }
}

fn pre_process_body(
    production: ProductionId,
    body: &ASTBody,
    symbols_table: &mut SymbolsTable,
    symbols_string_table: &mut SymbolStringTable,
) -> (Vec<Body>, Vec<Box<Production>>) {
    if let ASTNode::Returned(ret) = &body.reduce_function {
        // Extract the function and insert into function table?
        println!("{:?}", ret);
    }

    fn create_body_vectors(
        symbols: &Vec<ASTNode>,
        mut index: u32,
        symbols_table: &mut SymbolsTable,
        symbols_string_table: &mut SymbolStringTable,
    ) -> (Vec<Vec<BodySymbolRef>>, Vec<Box<Production>>) {
        let mut bodies = vec![];
        let mut productions = vec![];
        bodies.push(vec![]);

        for sym in symbols {
            let starting_bodies = bodies.len();

            fn create_production(
                name: &String,
                bodies: &Vec<ASTNode>,
                token: Token,
            ) -> (ASTNode, Box<Production>) {
                let name = String::from("test");

                // Create a virtual production and symbol to go in its place
                let symbol =
                    ASTNode::Production_Symbol(crate::grammar_data::ast::Production_Symbol::new(
                        name.clone(),
                        ASTNode::NONE,
                        0f64,
                        token,
                    ));

                let production = crate::grammar_data::ast::Production::new(
                    false,
                    symbol.clone(),
                    bodies.clone(),
                    false,
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
                sym_atom,
            } = get_symbol_details(sym);

            if let Some(mut sym) = sym_atom.to_owned() {
                let mut generated_symbol = ASTNode::NONE;
                if is_meta {
                    // Separate meta data symbols into it's own table that maps
                    // meta symbols to a body and its index.
                    index;
                    continue;
                }

                if is_group {
                    // Need to create new production that the virtual group production is bound
                    // to, add it to the list of currently considered productions, and replace
                    // this symbol with a production symbol pointing to the group.

                    // Except, if there are no functions within the production bodies
                    // we can simply inline the symbols into one or more alternate sets
                    // of bodies alongside the existing bodies.

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
                            // For each body in the group clone the existing body lists
                            // and process each list independently, inserting the new symbols
                            // into the existing bodies. We must make sure the indices are preserved
                            // since only the last symbol in each body can be bound to the index
                            // of the group production symbol.

                            let mut pending_bodies = vec![];

                            for body in &group.bodies {
                                if let ASTNode::Body(body) = body {
                                    let (mut new_bodies, mut new_productions) = create_body_vectors(
                                        &body.symbols,
                                        9999,
                                        symbols_table,
                                        symbols_string_table,
                                    );

                                    for body in new_bodies.iter_mut() {
                                        if let Some(last) = body.last_mut() {
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
                                    new_body.extend(pending_body.iter().cloned());
                                    new_bodies.push(new_body)
                                }
                            }

                            bodies = new_bodies;

                            index += 1;

                            // We do not to process the existing symbol
                            // so we'll skip the rest of the loop
                            continue;
                        } else {
                            let (prod_sym, production) = create_production(
                                &String::from("Test"),
                                &group.bodies,
                                group.tok.clone(),
                            );

                            productions.push(production);
                            generated_symbol = prod_sym;
                            sym = &generated_symbol;
                        }
                    } else {
                        panic!(
                            "\n{}",
                            sym.Token()
                                .blame(1, 1, "I don't know what to do with this!")
                                .unwrap_or(String::from("I don't know what to do with this!"))
                        )
                    }
                }

                if is_list {
                    // Create a new production that turns  A => a into A => a | A => A a
                    // produce a symbol id that points to that production

                    if let ASTNode::List_Production(list) = sym {
                        //Create new bodies that will be bound to the symbol.
                        let body_a = crate::grammar_data::ast::Body::new(
                            false,
                            vec![list.symbols.clone()],
                            None,
                            ASTNode::NONE,
                            None,
                        );

                        let mut body_b = body_a.clone();

                        match list.terminal_symbol {
                            ASTNode::NONE => {}
                            _ => {
                                body_b.symbols.insert(0, list.terminal_symbol.clone());
                            }
                        }

                        let (prod_sym, mut production) = create_production(
                            &String::from("Test"),
                            &vec![ASTNode::Body(body_b), ASTNode::Body(body_a)],
                            list.tok.clone(),
                        );

                        // Add the production symbol to the front of body be to make
                        // the body left recursive
                        if let ASTNode::Body(body) = &mut production.bodies[0] {
                            body.symbols.insert(0, prod_sym.clone());
                        }

                        productions.push(production);
                        generated_symbol = prod_sym;
                        sym = &generated_symbol;
                    } else {
                        panic!(
                            "\n{}",
                            sym.Token()
                                .blame(1, 1, "I don't know what to do with this!")
                                .unwrap_or(String::from("I don't know what to do with this!"))
                        )
                    }
                }

                if is_optional {
                    // Need to create new bodies that contains all permutations of encountered
                    // symbols except for the currently considered symbol. This is achieved by
                    // duplicating all body vecs, then adding the current symbol to the original
                    // vecs, but not the duplicates.
                    for entry in bodies.clone() {
                        bodies.push(entry)
                    }
                }

                let id = intern_symbol(sym, symbols_table, symbols_string_table);

                for i in 0..starting_bodies {
                    bodies[i].push(BodySymbolRef {
                        original_index: index,
                        sym_id: id.clone(),
                        annotation: annotation.clone(),
                        consumable: !is_no_consume,
                    });
                }

                index += 1;
            }
        }

        (bodies, productions)
    }

    let (bodies, productions) =
        create_body_vectors(&body.symbols, 0, symbols_table, symbols_string_table);

    (
        bodies
            .iter()
            .map(|b| Body {
                symbols: b.clone(),
                length: b.len() as u16,
                production,
                id: 0,
            })
            .collect(),
        productions,
    )
}

use std::{
    collections::{hash_map::DefaultHasher, VecDeque},
    hash::{Hash, Hasher},
};

fn hash_id_value<T: Hash>(t: T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

fn intern_symbol(
    sym: &ASTNode, /*, symbols_table, */
    symbols: &mut SymbolsTable,
    symbols_string_table: &mut SymbolStringTable,
) -> SymbolID {
    let mut id = SymbolID::Undefined;

    fn process_literal(
        string: &String,
        symbols: &mut SymbolsTable,
        symbols_string_table: &mut SymbolStringTable,
        exclusive: bool,
    ) -> SymbolID {
        let mut id = SymbolID::Undefined;
        let hash = hash_id_value(string);
        let identifier = Regex::new(r"[\w_-][\w\d_-]*$").unwrap();
        let number = Regex::new(r"\d+$").unwrap();
        let byte_length = string.bytes().len() as u32;
        let code_point_length = string.chars().count() as u32;

        if number.is_match(string) {
            id = SymbolID::DefinedNumeric(hash);
        } else if identifier.is_match(string) {
            id = SymbolID::DefinedIdentifier(hash);
        } else {
            id = SymbolID::DefinedGeneric(hash);
        }

        symbols_string_table.insert(id.clone(), string.to_owned());

        symbols.insert(
            id.clone(),
            Symbol {
                class_id: id.clone(),
                uuid: id.clone(),
                byte_length,
                code_point_length,
                scanner_index: 0,
                scanner_length: 0,
                exclusive,
            },
        );

        id
    }

    fn process_production(name: &String, symbols: &mut SymbolsTable) -> SymbolID {
        let mut id = SymbolID::Undefined;
        let hash = hash_id_value(name);
        id = SymbolID::Production(hash);
        symbols.insert(
            id.clone(),
            Symbol {
                class_id: id.clone(),
                uuid: id.clone(),
                byte_length: 0,
                code_point_length: 0,
                scanner_index: 0,
                scanner_length: 0,
                exclusive: false,
            },
        );

        id
    }

    fn process_token_production(
        name: &String,
        symbols: &mut SymbolsTable,
        exclusive: bool,
    ) -> SymbolID {
        let mut id = SymbolID::Undefined;
        let hash = hash_id_value(name);
        id = SymbolID::TokenProduction(hash);
        symbols.insert(
            id.clone(),
            Symbol {
                class_id: id.clone(),
                uuid: id.clone(),
                byte_length: 0,
                code_point_length: 0,
                scanner_index: 0,
                scanner_length: 0,
                exclusive,
            },
        );

        id
    }

    match sym {
        ASTNode::Generated(gen) => match gen.val.as_str() {
            "sp" => id = SymbolID::GenericSpace,
            "tab" => id = SymbolID::GenericHorizontalTab,
            "nl" => id = SymbolID::GenericNewLine,
            "id" => id = SymbolID::GenericIdentifier,
            "num" => id = SymbolID::GenericNumber,
            "sym" => id = SymbolID::GenericSymbol,
            "ids" => id = SymbolID::GenericIdentifiers,
            "nums" => id = SymbolID::GenericNumbers,
            "syms" => id = SymbolID::GenericSymbols,
            _ => {}
        },
        ASTNode::Exclusive_Literal(literal) => {
            id = process_literal(&literal.val, symbols, symbols_string_table, true)
        }
        ASTNode::Literal(literal) => {
            id = process_literal(&literal.val, symbols, symbols_string_table, false)
        }
        ASTNode::End_Of_File(_) => {
            id = SymbolID::EndOfFile;
        }
        ASTNode::Production_Symbol(prod) => {
            id = process_production(&prod.name, symbols);
        }
        ASTNode::Production_Import_Symbol(prod) => {
            panic!("Need to resolve imported production names!");
            let hash = hash_id_value(&prod.name);
            id = SymbolID::TokenProduction(hash);
        }
        ASTNode::Production_Token(prod) => match &prod.production {
            ASTNode::Production_Import_Symbol(prod) => {
                panic!("Need to resolve imported production names!");
                let hash = hash_id_value(&prod.name);
                id = SymbolID::TokenProduction(hash);
            }
            ASTNode::Production_Symbol(prod) => {
                id = process_token_production(&prod.name, symbols, false);
            }
            _ => {}
        },
        _ => {
            panic!(
                "Unexpected ASTNode while attempting to intern symbol \n{}",
                sym.Token()
                    .blame(0, 0, "found here")
                    .unwrap_or(String::new()),
            )
        }
    }

    return id;
}

struct SymbolData<'a> {
    pub annotation: String,
    pub is_list: bool,
    pub is_group: bool,
    pub is_optional: bool,
    pub is_no_consume: bool,
    pub is_meta: bool,
    pub sym_atom: Option<&'a ASTNode>,
}

/// Get a flattened view of a symbol's immediate AST
fn get_symbol_details<'a>(mut sym: &'a ASTNode) -> SymbolData<'a> {
    let mut data = SymbolData {
        annotation: String::new(),
        is_list: false,
        is_group: false,
        is_optional: false,
        is_no_consume: false,
        is_meta: false,
        sym_atom: None,
    };

    loop {
        match sym {
            ASTNode::AnnotatedSymbol(annotated) => {
                data.annotation = annotated.reference.val.to_owned();
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
            // This symbol types are "real" symbols, in as much
            // as they represent actual parsable entities which are
            // submitted to the bytecode compiler for evaluation
            ASTNode::Generated(_)
            | ASTNode::Literal(_)
            | ASTNode::Empty(_)
            | ASTNode::End_Of_File(_)
            | ASTNode::Exclusive_Literal(_)
            | ASTNode::Production_Symbol(_)
            | ASTNode::Production_Token(_)
            | ASTNode::Production_Import_Symbol(_) => {
                break;
            }
            _ => {
                panic!("Unexpected ASTNode {}", sym.GetType())
            }
        }
    }
    data.sym_atom = Some(sym);
    data
}

#[test]
fn test_pre_process_grammar() {
    let grammar = String::from(
        "\n@IMPORT ./test/me/out.hcg as bob \n<> a >  tk:p?^test a(+,) ( \\1234 | t:sp ? ( g:sp | g:sym g:sp ) f:r { basalt } ) \\nto <> a > tk:p p ",
    );
    if let Ok(grammar) = parse::compile_ast(&grammar) {
        pre_process_grammar(&grammar, &String::from("/grammar"));
    } else {
        panic!("Failed to parse and produce an AST of '<> a > b'");
    }
}
