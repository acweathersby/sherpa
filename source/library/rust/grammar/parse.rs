use std::fmt::{Display, Error};

use crate::{
    primitives::HCObj,
    runtime::{buffer::UTF8StringReader, completer::complete, recognizer::iterator::*},
};

use super::grammar_data::{
    ast::{ASTNode, FunctionMaps, Grammar},
    parser_data::{EntryPoint_Hc, BYTECODE},
};

pub fn parse_string(string: &String) {}
#[derive(Debug)]

pub enum ParseError {
    UNDEFINED,
    IO_ERROR(std::io::Error),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UNDEFINED => f.write_str("An unknown error has occured "),
            ParseError::IO_ERROR(err) => err.fmt(f),
        }
    }
}

pub fn compile_ast(buffer: Vec<u8>) -> Result<Box<Grammar>, ParseError> {
    let mut iterator: ReferenceIterator<UTF8StringReader> =
        ReferenceIterator::new(UTF8StringReader::new(buffer), EntryPoint_Hc, &BYTECODE);

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
    let input = String::from("\n<> a > b\n");
    let result = compile_ast(Vec::from(input.as_bytes()));
    assert!(result.is_ok());
}

#[test]
fn test_production_with_generic_symbol() {
    let input = String::from("\n<> a > g:sp\n");
    let result = compile_ast(Vec::from(input.as_bytes()));
    assert!(result.is_ok());
}
