mod ast;
mod parser_data;

use crate::ast::ASTNode;
use crate::ast::REDUCE_FUNCTIONS;
use crate::parser_data::EntryPoint_default;
use crate::parser_data::BYTECODE;
use hctk::types::UTF8StringReader;
use hctk::types::*;
use hctk::types::*;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;
pub fn main()
{
    let reader = UTF8StringReader::from_string("hello world");

    let iter =
        ReferenceParseIterator::new(reader, &BYTECODE, EntryPoint_default);
    let mut nodes: Vec<HCObj<ASTNode>> = Vec::with_capacity(8);
    let mut messages = Vec::<String>::with_capacity(10);

    let start = Instant::now();

    for action in iter {
        match action {
            ParseAction::Shift {
                skipped_characters: skip,
                token,
            } => {
                messages.push(format!(
                    "Skip {:? } & Extract token {:?} ",
                    skip, token
                ));
            }
            ParseAction::Accept { production_id } => {
                messages.push(format!("Accept production {}", production_id));
                break;
            }
            _ => {}
        }
    }

    let duration = start.elapsed();

    messages.iter().for_each(|s| println!("{}", s));

    println!("-- dur: {:?}", duration)
}
