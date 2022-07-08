use std::fmt::Display;
use std::fmt::Error;
use std::sync::PoisonError;

use crate::deprecated_runtime::buffer::UTF8StringReader;
use crate::deprecated_runtime::completer::complete;
use crate::deprecated_runtime::recognizer::iterator::*;

use crate::types::ParseError;
use crate::types::Token;

use super::data::ast::ASTNode;
use super::data::ast::FunctionMaps;
use super::data::ast::Grammar;
use super::data::ast::IR_STATE;
use super::data::ast_node::HCObj;
use super::data::parser_data::EntryPoint_Hc;
use super::data::parser_data::EntryPoint_Ir;
use super::data::parser_data::EntryPoint_Script;
use super::data::parser_data::BYTECODE;

pub fn compile_grammar_ast(buffer: Vec<u8>)
    -> Result<Box<Grammar>, ParseError>
{
    let mut iterator: ReferenceIterator<UTF8StringReader> =
        ReferenceIterator::new(
            UTF8StringReader::new(buffer),
            EntryPoint_Hc,
            &BYTECODE,
        );

    let result = complete(&mut iterator, &FunctionMaps);

    match result {
        Ok(result) => {
            if let HCObj::NODE(ASTNode::Grammar(node)) = result {
                Ok(node)
            } else {
                Err(ParseError::UNDEFINED)
            }
        }
        Err(err) => Err(err),
    }
}

pub fn compile_ir_ast(buffer: Vec<u8>) -> Result<Box<IR_STATE>, ParseError>
{
    let mut iterator: ReferenceIterator<UTF8StringReader> =
        ReferenceIterator::new(
            UTF8StringReader::new(buffer),
            EntryPoint_Ir,
            &BYTECODE,
        );

    let result = complete(&mut iterator, &FunctionMaps);

    match result {
        Ok(result) => {
            if let HCObj::NODE(ASTNode::IR_STATE(node)) = result {
                Ok(node)
            } else {
                Err(ParseError::UNDEFINED)
            }
        }
        Err(err) => Err(err),
    }
}

pub fn compile_ascript_ast(buffer: Vec<u8>) -> Result<ASTNode, ParseError>
{
    let mut iterator: ReferenceIterator<UTF8StringReader> =
        ReferenceIterator::new(
            UTF8StringReader::new(buffer),
            EntryPoint_Script,
            &BYTECODE,
        );

    let result = complete(&mut iterator, &FunctionMaps);

    match result {
        Ok(result) => {
            if let HCObj::NODE(node) = result {
                Ok(node)
            } else {
                Err(ParseError::UNDEFINED)
            }
        }
        Err(err) => Err(ParseError::UNDEFINED),
    }
}

#[test]

fn test_ir_trivial_state()
{
    let input = String::from("state [ A ] \n pass");

    let result = compile_ir_ast(Vec::from(input.as_bytes()));

    assert!(result.is_ok());
}

#[test]

fn test_ir_trivial_branch_state()
{
    let input =
        String::from("state [ A ] assert TOKEN [ /* a */ 11 ] ( pass )");

    let result = compile_ir_ast(Vec::from(input.as_bytes()));

    assert!(result.is_ok());

    print!("{:#?}", result.unwrap());
}

#[test]

fn test_trivial_ascript_struct()
{
    let input = String::from(" { t_Type, t_Class, value: u32 } ");

    let result = compile_ascript_ast(Vec::from(input.as_bytes()));

    print!("{:#?}", result);

    assert!(result.is_ok());
}

#[test]

fn test_production_minimum()
{
    let input = String::from("\n<> a > b\n");

    let result = compile_grammar_ast(Vec::from(input.as_bytes()));

    assert!(result.is_ok());
}

#[test]

fn test_production_with_generic_symbol()
{
    let input = String::from("\n<> a > g:sp\n");

    let result = compile_grammar_ast(Vec::from(input.as_bytes()));

    assert!(result.is_ok());
}
