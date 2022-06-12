use std::fmt::Display;
use std::fmt::Error;
use std::sync::PoisonError;

use crate::primitives::HCObj;
use crate::primitives::Token;
use crate::runtime::buffer::UTF8StringReader;
use crate::runtime::completer::complete;
use crate::runtime::error::TokenError;
use crate::runtime::recognizer::iterator::*;

use super::data::ast::ASTNode;
use super::data::ast::FunctionMaps;
use super::data::ast::Grammar;
use super::data::ast::IR_STATE;
use super::data::parser_data::EntryPoint_Hc;
use super::data::parser_data::EntryPoint_Ir;
use super::data::parser_data::BYTECODE;

pub fn parse_string(string: &String) {}

/// Indicates an issue encountered while processing a grammar
/// symbol.
#[derive(Debug)]
pub struct CompileProblem
{
    // Message that appears
    pub message:        String,
    /// Message that appears inline the code location
    /// diagram
    pub inline_message: String,
    pub loc:            Token,
}

impl Display for CompileProblem
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        f.write_fmt(format_args!(
            "{}\n{}",
            self.message,
            self.loc
                .blame(1, 1, &self.inline_message)
                .unwrap_or("".to_string()),
        ))
    }
}
/// Indicates an issue encountered while processing grammar
/// symbols that effects multiple locations in one or more files.
#[derive(Debug)]
pub struct CompoundCompileProblem
{
    pub message:   String,
    pub locations: Vec<CompileProblem>,
}

impl Display for CompoundCompileProblem
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        f.write_fmt(format_args!(
            "{}\n{}",
            self.message,
            self.locations
                .iter()
                .map(|s| format!("{}", s))
                .collect::<Vec<_>>()
                .join("\n"),
        ))
    }
}

#[derive(Debug)]
pub enum ParseError
{
    UNDEFINED,
    IO_ERROR(std::io::Error),
    MUTEX_ERROR,
    THREAD_ERROR,
    COMPILE_PROBLEM(CompileProblem),
    TOKEN_ERROR(TokenError),
    COMPOUND_COMPILE_PROBLEM(CompoundCompileProblem),
}

impl Display for ParseError
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        match self {
            ParseError::UNDEFINED => {
                f.write_str("An unknown error has occurred ")
            }
            ParseError::TOKEN_ERROR(err) => err.fmt(f),
            ParseError::IO_ERROR(err) => err.fmt(f),
            ParseError::MUTEX_ERROR => f.write_str("A Mutex has been poisoned"),
            ParseError::THREAD_ERROR => {
                f.write_str("Unable to get an exclusive lock on an object")
            }
            ParseError::COMPOUND_COMPILE_PROBLEM(err) => err.fmt(f),
            ParseError::COMPILE_PROBLEM(err) => err.fmt(f),
        }
    }
}

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
        Ok(r) => {
            if let HCObj::NODE(node) = r {
                if let ASTNode::Grammar(node) = node {
                    Ok(node)
                } else {
                    Err(ParseError::UNDEFINED)
                }
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
        Ok(r) => {
            if let HCObj::NODE(node) = r {
                if let ASTNode::IR_STATE(node) = node {
                    Ok(node)
                } else {
                    Err(ParseError::UNDEFINED)
                }
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
