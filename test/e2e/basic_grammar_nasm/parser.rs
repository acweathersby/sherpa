
//! ### `grammar` Parser Data
//!
//! - **GENERATOR** :   hc-compile 0.2.0
//! - **SOURCE FILE**:  grammar
//!
//! #### WARNING:
//!
//! This is a generated file. Any changes to this file may be
//! overwritten without notice.
//!
//! #### Copyright
//! 
//! (C) 2022 Anthony Weathersby and the Hydrocarbon Toolkit Authors.


    
use hctk::types::*;

use hctk::types::*;

type AnonymousPtr = u64;

#[link(name = "grammar_parser")]
extern "C" {
    fn construct_context(ctx: AnonymousPtr);
    fn next(ctx: AnonymousPtr) -> i32;
    fn destroy_context(ctx: AnonymousPtr);
    fn prime_context(ctx: AnonymousPtr);
}

pub struct MyParser<T: SymbolReader>(ASMParserContext<T>);

impl<T: SymbolReader> Iterator for MyParser<T> {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            let _ptr = &mut self.0 as *const ASMParserContext<T>;
            Some(next(_ptr as u64))
        }
    }
}

impl<T: SymbolReader> MyParser<T> {
    pub fn new(reader: &mut T) -> Self {
        let mut parser = Self(ASMParserContext::<T>::new(reader));
        parser.construct_context();
        parser
    }

    fn construct_context(&mut self) {
        unsafe {
            let _ptr = &mut self.0 as *const ASMParserContext<T>;
            construct_context(_ptr as u64);
        }
    }

    fn destroy_context(&mut self) {
        unsafe {
            let _ptr = &mut self.0 as *const ASMParserContext<T>;
            destroy_context(_ptr as u64);
        };
    }
}

impl<T: SymbolReader> Drop for MyParser<T> {
    fn drop(&mut self) {
        self.destroy_context();
    }
}