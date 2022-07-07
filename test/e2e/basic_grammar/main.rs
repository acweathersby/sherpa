mod ast;

use crate::ast::ASTNode;
use crate::ast::REDUCE_FUNCTIONS;
use hctk::types::UTF8StringReader;
use hctk::types::*;
use hctk::types::*;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;
use test_basic_grammar::*;

pub fn main()
{
    let mut nodes: Vec<HCObj<ASTNode>> = Vec::with_capacity(8);
    let mut messages = Vec::<String>::with_capacity(10);

    let start = Instant::now();

    for action in Context::new_banner_parser(&mut UTF8StringReader::new(
        "hello world".to_string(),
    )) {
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
