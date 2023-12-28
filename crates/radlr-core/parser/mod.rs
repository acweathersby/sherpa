#![allow(warnings)]
#![allow(unused, improper_ctypes)]
#![allow(bad_style)]
#![allow(non_snake_case)]

#[cfg(feature = "binary-parser")]
pub(crate) mod radlr;

#[cfg(feature = "binary-parser")]
pub use radlr::{Goto, Range, *};

#[cfg(not(feature = "binary-parser"))]
pub mod radlr_bc;

#[cfg(not(feature = "binary-parser"))]
pub use radlr_bc::{Goto, Range, *};
