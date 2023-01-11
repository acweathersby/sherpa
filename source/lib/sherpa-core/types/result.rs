//! Global catch-all for errors.
//! Methods for reporting errors are provided

use crate::types::*;
use std::{
  convert::Infallible,
  ops::{ControlFlow, FromResidual, Try},
  panic::Location,
  process::{ExitCode, Termination},
};

/// A result type that uses the [SherpaError] enum type for errors results.
#[derive(Debug)]
pub enum SherpaResult<T> {
  /// TODO: Docs
  Ok(T),
  /// TODO: Docs
  Err(SherpaError),
  /// TODO: Docs
  MultipleErrors(Vec<SherpaError>),
  /// TODO: Docs
  None,
}

impl<T> SherpaResult<T> {
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
    matches!(self, SherpaResult::Err(_) | SherpaResult::MultipleErrors(_))
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
      SherpaResult::None => panic!("called `SherpaResult::unwrap()` on an `None` value"),
      SherpaResult::Err(err) => {
        panic!("called `SherpaResult::unwrap()` on an `Err` value: \n {}", err)
      }
      SherpaResult::MultipleErrors(errors) => {
        panic!(
          "called `SherpaResult::unwrap()` on an `MultipleErrors` value: \n {:#?}",
          errors.debug_print()
        )
      }
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
      SherpaResult::MultipleErrors(errors) => {
        Err(SherpaError::Many { message: Default::default(), errors: errors })
      }
      _ => Err(SherpaError::UNDEFINED),
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
      SherpaResult::MultipleErrors(err) => SherpaResult::MultipleErrors(err),
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
      SherpaResult::MultipleErrors(e) => ControlFlow::Break(SherpaResult::MultipleErrors(e)),
      SherpaResult::None => ControlFlow::Break(SherpaResult::None),
    }
  }
}

impl<T> Termination for SherpaResult<T> {
  #[inline]
  #[track_caller]
  fn report(self) -> std::process::ExitCode {
    match self {
      SherpaResult::MultipleErrors(errors) => {
        errors.stderr_print();
        ExitCode::FAILURE
      }
      SherpaResult::Err(error) => {
        eprintln!("{}", error);
        ExitCode::FAILURE
      }
      SherpaResult::None => {
        eprintln!("No Results");
        ExitCode::FAILURE
      }
      SherpaResult::Ok(_) => ExitCode::SUCCESS,
    }
  }
}
