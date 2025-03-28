//! Functions for dealing with Options and Results

use crate::types::*;

/// A result type that uses the [RadlrError] enum type for errors values.
pub type RadlrResult<T> = std::result::Result<T, RadlrError>;

/// Converts an Option to a RadlrResult
pub fn o_to_r<T>(result: Option<T>, error_msg: &str) -> RadlrResult<T> {
  match result {
    Some(r) => Ok(r),
    None => Err(RadlrError::Text(error_msg.to_string())),
  }
}
