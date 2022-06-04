use hctk::{
    primitives::HCObj,
    runtime::{buffer::UTF8StringReader, completer::complete, recognizer::iterator::*},
};

use crate::grammar_data::{
    ast::{ASTNode, FunctionMaps, Grammar},
    parser_data::{EntryPoint_Hc, BYTECODE},
};

pub fn parse_string(string: &String) {}

pub enum ParseError {
    UNDEFINED,
    UNABLE_TO_DEREF_QUERY_RESULT,
}

pub fn compile_ast(string: &String) -> Result<Box<Grammar>, ParseError> {
    let mut iterator: ReferenceIterator<UTF8StringReader> = ReferenceIterator::new(
        UTF8StringReader::new(Vec::from(string.as_bytes())),
        EntryPoint_Hc,
        &BYTECODE,
    );

    let result = complete(&mut iterator, &FunctionMaps);

    match result {
        Ok(r) => {
            //println!("{:?}", r);
            if let HCObj::NODE(node) = r {
                if let ASTNode::Grammar(node) = node {
                    return Ok(node);
                }
            }
        }
        Err(err) => {
            println!("{:?}", err);
        }
    }

    Err(ParseError::UNDEFINED)
}

#[test]
fn test_production_minimum() {
    let input = String::from("\n\n<> a > b\n");
    let result = compile_ast(&input);
    assert!(result.is_ok());
    if let Ok(grammar) = result {
        if let Some(blame) = grammar.tok.blame(1, 1) {
            println!("{}", blame);
        }
    }
}
