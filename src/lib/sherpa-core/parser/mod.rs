#![allow(unused, improper_ctypes)]
#![allow(bad_style)]
#![allow(non_snake_case)]

#[cfg(feature = "binary-parser")]
pub(crate) mod sherpa;

#[cfg(feature = "binary-parser")]
pub use sherpa::{Goto, Range, *};

#[cfg(not(feature = "binary-parser"))]
pub mod sherpa_bc;

#[cfg(not(feature = "binary-parser"))]
pub use sherpa_bc::{Goto, Range, *};
