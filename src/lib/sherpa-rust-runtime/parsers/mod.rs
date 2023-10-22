use self::{
  ast::{Node, Tk},
  fork::ForkableParser,
  recognizer::Recognizer,
  token::TokenProducer,
};
use crate::types::{ParserInitializer, ParserInput, ParserIterator};

//pub mod cst;
pub mod ast;
pub mod error_recovery;
pub mod fork;
pub mod recognizer;
pub mod token;

pub trait Parser<T: ParserInput>: ParserIterator<T> + Recognizer<T> + TokenProducer<T> + ForkableParser<T> {}
impl<I: ParserInput, P: ParserIterator<I> + ParserInitializer> Parser<I> for P {}
//impl<I: ParserInput, P: ForkableParser<I>> ErrorRecoveryParser<I> for P {}
