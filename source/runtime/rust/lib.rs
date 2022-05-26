pub mod ast;
pub mod error;
mod token;
mod buffer;
mod completer;
mod recognizer;
mod utf8;
pub use token::*;
pub use buffer::*;
pub use completer::*;
pub use lazy_static::lazy_static;
pub use recognizer::*;
use utf8::*;

pub const fn get_version_string() -> &'static str {
    "HCToolkit Runtime Library 2022 v0.0.1"
}
