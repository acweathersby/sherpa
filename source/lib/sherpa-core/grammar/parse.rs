use super::data::{
  ast::{ASTNode, FunctionMaps, Grammar, IR_STATE},
  ast_node::HCObj,
  parser_data::{EntryPoint_Hc, EntryPoint_Ir, EntryPoint_Script, BYTECODE},
};
use crate::{
  deprecated_runtime::{buffer::UTF8StringReader, completer::complete, recognizer::iterator::*},
  types::{SherpaError, Token},
};
use std::{
  fmt::{Display, Error},
  sync::PoisonError,
};

pub fn compile_grammar_ast(buffer: Vec<u8>) -> Result<Box<Grammar>, SherpaError> {
  let mut iterator: ReferenceIterator<UTF8StringReader> =
    ReferenceIterator::new(UTF8StringReader::new(buffer), EntryPoint_Hc, &BYTECODE);

  let result = complete(&mut iterator, &FunctionMaps);

  match result {
    Ok(result) => {
      if let HCObj::NODE(ASTNode::Grammar(node)) = result {
        Ok(node)
      } else {
        Err(SherpaError::UNDEFINED)
      }
    }
    Err(err) => Err(err),
  }
}

pub fn compile_ir_ast(buffer: Vec<u8>) -> Result<Box<IR_STATE>, SherpaError> {
  let mut iterator: ReferenceIterator<UTF8StringReader> =
    ReferenceIterator::new(UTF8StringReader::new(buffer), EntryPoint_Ir, &BYTECODE);

  let result = complete(&mut iterator, &FunctionMaps);

  match result {
    Ok(result) => {
      if let HCObj::NODE(ASTNode::IR_STATE(node)) = result {
        Ok(node)
      } else {
        Err(SherpaError::UNDEFINED)
      }
    }
    Err(err) => Err(err),
  }
}

pub fn compile_ascript_ast(buffer: Vec<u8>) -> Result<ASTNode, SherpaError> {
  let mut iterator: ReferenceIterator<UTF8StringReader> =
    ReferenceIterator::new(UTF8StringReader::new(buffer), EntryPoint_Script, &BYTECODE);

  let result = complete(&mut iterator, &FunctionMaps);

  match result {
    Ok(result) => {
      if let HCObj::NODE(node) = result {
        Ok(node)
      } else {
        Err(SherpaError::UNDEFINED)
      }
    }
    Err(err) => Err(SherpaError::UNDEFINED),
  }
}
