//! Global catch-all for errors.
//! Methods for reporting errors are provided

use crate::types::*;
use std::{
  convert::Infallible,
  fmt::Display,
  ops::{ControlFlow, FromResidual, Try},
  panic::Location,
  process::{ExitCode, Termination},
};

/// A result type that uses the [SherpaError] enum type for errors results.
#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum SherpaResult<T> {
  /// The resolved value.
  Ok(T),
  /// An error that occured
  Err(SherpaError),
  /// Could no resolve value
  None,
}

impl<T> SherpaResult<T> {
  /// Return a SherpaResult containing a mutable reference
  /// to the original stored object, or a faulty result if
  /// the original SherpaResult is faulty
  pub fn as_mut(&mut self) -> SherpaResult<&mut T> {
    match self {
      Self::Ok(val) => SherpaResult::Ok(val),
      Self::None => SherpaResult::None,
      Self::Err(err) => SherpaResult::Err(err.clone()),
    }
  }

  /// Return a SherpaResult containing a reference
  /// to the original stored object, or a faulty result if
  /// the original SherpaResult is faulty
  pub fn as_ref(&self) -> SherpaResult<&T> {
    match self {
      Self::Ok(val) => SherpaResult::Ok(val),
      Self::None => SherpaResult::None,
      Self::Err(err) => SherpaResult::Err(err.clone()),
    }
  }

  /// Returns `true` if the result is `Ok`
  ///
  /// # Examples
  ///
  /// Basic usage:
  /// ```
  /// 
  /// # use sherpa_core::SherpaResult;
  ///
  /// let x = SherpaResult::<u32>::None;
  /// assert_eq!(x.is_ok(), false);
  ///
  /// let x = SherpaResult::Ok(2222u32);
  /// assert_eq!(x.is_ok(), true);
  ///
  /// let x = SherpaResult::<u32>::Err("Bad Data".into());
  /// assert_eq!(x.is_ok(), false);
  ///
  /// let x: SherpaResult<i32> = SherpaResult::Ok(2);
  /// assert_eq!(x.is_ok(), true);
  /// ```
  #[inline]
  pub fn is_ok(&self) -> bool {
    matches!(self, SherpaResult::Ok(_))
  }

  /// Returns a string representation of the error(s) contained
  pub fn get_error_string(&self) -> String {
    match self {
      SherpaResult::Ok(_) => format!("No Error"),
      SherpaResult::None => format!("SherpaResult is None"),
      SherpaResult::Err(err) => err.to_string(),
    }
  }

  /// Returns `true` if the result is `Err`
  ///
  /// # Examples
  ///
  /// Basic usage:
  /// ```
  /// # use sherpa_core::SherpaResult;
  ///
  /// let x = SherpaResult::<u32>::None;
  /// assert_eq!(x.is_err(), false);
  ///
  /// let x = SherpaResult::Ok(2222u32);
  /// assert_eq!(x.is_err(), false);
  ///
  /// let x = SherpaResult::<u32>::Err("Bad Data".into());
  /// assert_eq!(x.is_err(), true);
  ///
  /// let x: SherpaResult<i32> = SherpaResult::Ok(2);
  /// assert_eq!(x.is_err(), false);
  /// ```
  #[inline]
  pub fn is_err(&self) -> bool {
    matches!(self, SherpaResult::Err(_))
  }

  /// Returns `true` if the result is `None`
  ///
  /// # Examples
  ///
  /// Basic usage:
  /// ```
  /// # use sherpa_core::SherpaResult;
  ///
  /// let x = SherpaResult::<u32>::None;
  /// assert_eq!(x.is_none(), true);
  ///
  /// let x = SherpaResult::Ok(2222u32);
  /// assert_eq!(x.is_none(), false);
  ///
  /// let x = SherpaResult::<u32>::Err("Bad Data".into());
  /// assert_eq!(x.is_none(), false);
  ///
  /// let x: SherpaResult<i32> = SherpaResult::Ok(2);
  /// assert_eq!(x.is_none(), false);
  /// ```
  #[inline]
  pub fn is_none(&self) -> bool {
    matches!(self, SherpaResult::None)
  }

  /// Returns `true` if the result is `Err` or `None`
  ///
  /// # Examples
  ///
  /// Basic usage:
  /// ```
  /// # use sherpa_core::SherpaResult;
  ///
  /// let x = SherpaResult::<u32>::None;
  /// assert_eq!(x.is_faulty(), true);
  ///
  /// let x = SherpaResult::Ok(2222u32);
  /// assert_eq!(x.is_faulty(), false);
  ///
  /// let x = SherpaResult::<u32>::Err("Bad Data".into());
  /// assert_eq!(x.is_faulty(), true);
  ///
  /// let x: SherpaResult<i32> = SherpaResult::Ok(2);
  /// assert_eq!(x.is_faulty(), false);
  /// ```
  #[inline]
  pub fn is_faulty(&self) -> bool {
    self.is_err() || self.is_none()
  }

  /// Returns the contained [`Ok`] value, consuming the `self` value.
  ///
  /// # Panics
  ///
  /// Panics if the self value equals [`None`] or [`Err`].
  #[inline]
  #[track_caller]
  pub fn unwrap(self) -> T {
    match self {
      SherpaResult::Ok(val) => val,
      SherpaResult::None => {
        panic!("called `SherpaResult::unwrap()` on an `None` value")
      }
      SherpaResult::Err(err) => {
        panic!("called `SherpaResult::unwrap()` on an `Err` value: \n {}", err)
      }
    }
  }

  /// Convert the SherpaResult into an Option, discarding any
  /// errors if the result is a SherpaResult::Err type.
  pub fn to_option(self) -> Option<T> {
    match self {
      SherpaResult::Ok(val) => Some(val),
      SherpaResult::None => None,
      SherpaResult::Err(err) => None,
    }
  }
}

impl<T: Display> Display for SherpaResult<T>
where
  T: Display,
{
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      SherpaResult::Ok(obj) => {
        f.write_str("SherpaResult::Ok{\n")?;
        obj.fmt(f)?;
        f.write_str("\n}")
      }
      SherpaResult::Err(err) => {
        f.write_str("SherpaResult::Err{\n")?;
        err.fmt(f)?;
        f.write_str("\n}")
      }

      SherpaResult::None => f.write_str("SherpaResult::None"),
    }
  }
}

impl<T, E> From<Result<T, E>> for SherpaResult<T>
where
  SherpaError: From<E>,
{
  #[inline]
  #[track_caller]
  fn from(other: Result<T, E>) -> Self {
    match other {
      Err(e) => SherpaResult::Err(SherpaError::from(e)),
      Ok(v) => SherpaResult::Ok(v),
    }
  }
}

impl<T, E> FromResidual<Result<Infallible, E>> for SherpaResult<T>
where
  SherpaError: From<E>,
{
  #[inline]
  #[track_caller]
  fn from_residual(residual: Result<Infallible, E>) -> Self {
    match residual {
      Err(e) => SherpaResult::Err(SherpaError::from(e)),
      _ => SherpaResult::from_residual(residual),
    }
  }
}

impl<T> FromResidual<SherpaResult<Infallible>> for Result<T, SherpaError> {
  #[inline]
  #[track_caller]
  fn from_residual(residual: SherpaResult<Infallible>) -> Self {
    match residual {
      SherpaResult::Err(err) => Err(err),
      _ => Err("UNDEFINED ERROR".into()),
    }
  }
}

impl<T> FromResidual<Option<Infallible>> for SherpaResult<T> {
  #[inline]
  #[track_caller]
  fn from_residual(residual: Option<Infallible>) -> Self {
    match residual {
      Option::None => SherpaResult::Err(SherpaError::from(format!(
        "Tried to to access the value of an Option::None:\n    {}",
        Location::caller().to_string(),
      ))),
      _ => SherpaResult::from_residual(residual),
    }
  }
}

impl<T> FromResidual<SherpaResult<Infallible>> for SherpaResult<T> {
  #[inline]
  #[track_caller]
  fn from_residual(residual: SherpaResult<Infallible>) -> Self {
    match residual {
      SherpaResult::Err(err) => SherpaResult::Err(err),
      _ => SherpaResult::None,
    }
  }
}

impl<T> Try for SherpaResult<T> {
  type Output = T;
  type Residual = SherpaResult<Infallible>;

  #[inline]
  #[track_caller]
  fn from_output(output: Self::Output) -> Self {
    SherpaResult::Ok(output)
  }

  #[inline]
  #[track_caller]
  fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
    match self {
      SherpaResult::Ok(v) => ControlFlow::Continue(v),
      SherpaResult::Err(e) => ControlFlow::Break(SherpaResult::Err(e)),
      SherpaResult::None => ControlFlow::Break(SherpaResult::None),
    }
  }
}

impl<T> Termination for SherpaResult<T> {
  #[inline]
  #[track_caller]
  fn report(self) -> std::process::ExitCode {
    match self {
      SherpaResult::Err(error) => {
        println!("{}", error);
        ExitCode::FAILURE
      }
      SherpaResult::None => {
        println!("No Results");
        ExitCode::FAILURE
      }
      SherpaResult::Ok(_) => ExitCode::SUCCESS,
    }
  }
}
