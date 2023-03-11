#![allow(unused, improper_ctypes)]
#[cfg(not(feature = "sherpa-binary-parser"))]
pub mod sherpa_bc;
#[cfg(not(feature = "sherpa-binary-parser"))]
pub use sherpa_bc::*;
