//! Functions for dealing with Options and Results

use crate::types::*;

/// A result type that uses the [SherpaError] enum type for errors values.
pub type SherpaResult<T: Default> = std::result::Result<T, SherpaError>;

pub const fn SherpaResult_None<T>() -> SherpaResult<T> {
  SherpaResult::Err(SherpaError::StaticText("Undefined Error"))
}

pub const fn SherpaResult_Err<T>(text: &'static str) -> SherpaResult<T> {
  SherpaResult::Err(SherpaError::StaticText(text))
}

/// Converts an Option to a SherpaResult
pub fn o_to_r<T>(result: Option<T>, error_msg: &'static str) -> SherpaResult<T> {
  match result {
    Some(r) => Ok(r),
    None => Err(SherpaError::StaticText(error_msg)),
  }
}
