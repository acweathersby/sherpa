#![crate_type = "rlib"]
#![feature(const_eval_limit)]
#![const_eval_limit = "0"]
#![feature(new_uninit)]
#![feature(get_mut_unchecked)]
#![feature(scoped_threads)]

pub mod primitives;
pub mod runtime;
pub mod utf8;
pub use lazy_static::lazy_static;
pub mod debug;
pub mod grammar;
pub mod intermediate;
