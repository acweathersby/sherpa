//! Global catch-all for errors.
//! Methods for reporting errors are provided

use std::convert;
use std::convert::Infallible;
use std::fmt::Display;
use std::fmt::Error;
use std::ops::ControlFlow;
use std::ops::FromResidual;
use std::ops::Residual;
use std::ops::Try;
use std::path::Path;
use std::path::PathBuf;
use std::process::ExitCode;
use std::process::Termination;
use std::sync::Arc;

use crate::compile_grammar_from_string;
use crate::types::*;

/// A Result type that uses the HCTKError enum.
// pub type HCResult<T> = Result<T, HCError>;

#[derive(Debug)]
pub enum HCResult<T> {
  Ok(T),
  Err(HCError),
  MultipleErrors(Vec<HCError>),
  None,
}

impl<T> HCResult<T> {
  /// Returns `true` if the result is `Ok`
  ///
  /// # Examples
  ///
  /// Basic usage:
  /// ```
  /// # use hctk_core::debug::HCResult;
  ///
  /// let x = HCResult::from(Option::<u32>::None);
  /// assert_eq!(x.is_ok(), false);
  ///
  /// let x = HCResult::from(Option::Some(2222u32));
  /// assert_eq!(x.is_ok(), true);
  ///
  /// let x = HCResult::from(Result::<u32, &str>::Err("Bad Data"));
  /// assert_eq!(x.is_ok(), false);
  ///
  /// let x: HCResult<i32> = HCResult::Ok(2);
  /// assert_eq!(x.is_ok(), true);
  /// ```
  #[inline]
  pub fn is_ok(&self) -> bool {
    matches!(self, HCResult::Ok(_))
  }

  /// Returns `true` if the result is `Err`
  ///
  /// # Examples
  ///
  /// Basic usage:
  /// ```
  /// # use hctk_core::debug::HCResult;
  ///
  /// let x = HCResult::from(Option::<u32>::None);
  /// assert_eq!(x.is_err(), false);
  ///
  /// let x = HCResult::from(Option::Some(2222u32));
  /// assert_eq!(x.is_err(), false);
  ///
  /// let x = HCResult::from(Result::<u32, &str>::Err("Bad Data"));
  /// assert_eq!(x.is_err(), true);
  ///
  /// let x: HCResult<i32> = HCResult::Ok(2);
  /// assert_eq!(x.is_err(), false);
  /// ```
  #[inline]
  pub fn is_err(&self) -> bool {
    matches!(self, HCResult::Err(_) | HCResult::MultipleErrors(_))
  }

  /// Returns `true` if the result is `None`
  ///
  /// # Examples
  ///
  /// Basic usage:
  /// ```
  /// # use hctk_core::debug::HCResult;
  ///
  /// let x = HCResult::from(Option::<u32>::None);
  /// assert_eq!(x.is_none(), true);
  ///
  /// let x = HCResult::from(Option::Some(2222u32));
  /// assert_eq!(x.is_none(), false);
  ///
  /// let x = HCResult::from(Result::<u32, &str>::Err("Bad Data"));
  /// assert_eq!(x.is_none(), false);
  ///
  /// let x: HCResult<i32> = HCResult::Ok(2);
  /// assert_eq!(x.is_none(), false);
  /// ```
  #[inline]
  pub fn is_none(&self) -> bool {
    matches!(self, HCResult::None)
  }

  /// Returns `true` if the result is `Err` or `None`
  ///
  /// # Examples
  ///
  /// Basic usage:
  /// ```
  /// # use hctk_core::debug::HCResult;
  ///
  /// let x = HCResult::from(Option::<u32>::None);
  /// assert_eq!(x.is_faulty(), true);
  ///
  /// let x = HCResult::from(Option::Some(2222u32));
  /// assert_eq!(x.is_faulty(), false);
  ///
  /// let x = HCResult::from(Result::<u32, &str>::Err("Bad Data"));
  /// assert_eq!(x.is_faulty(), true);
  ///
  /// let x: HCResult<i32> = HCResult::Ok(2);
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
      HCResult::Ok(val) => val,
      HCResult::None => panic!("called `HCResult::unwrap()` on an `None` value"),
      HCResult::Err(err) => panic!("called `HCResult::unwrap()` on an `Err` value: \n {}", err),
      HCResult::MultipleErrors(errors) => {
        panic!(
          "called `HCResult::unwrap()` on an `MultipleErrors` value: \n {:#?}",
          errors.debug_print()
        )
      }
    }
  }
}

impl<T, E> From<Result<T, E>> for HCResult<T>
where
  HCError: From<E>,
{
  #[inline]
  #[track_caller]
  fn from(other: Result<T, E>) -> Self {
    match other {
      Err(e) => HCResult::Err(HCError::from(e)),
      Ok(v) => HCResult::Ok(v),
    }
  }
}

impl<T, E> FromResidual<Result<Infallible, E>> for HCResult<T>
where
  HCError: From<E>,
{
  #[inline]
  #[track_caller]
  fn from_residual(residual: Result<Infallible, E>) -> Self {
    match residual {
      Err(e) => HCResult::Err(HCError::from(e)),
      _ => HCResult::from_residual(residual),
    }
  }
}

impl<T> FromResidual<HCResult<Infallible>> for Result<T, HCError> {
  #[inline]
  #[track_caller]
  fn from_residual(residual: HCResult<Infallible>) -> Self {
    match residual {
      HCResult::Err(err) => Err(err),
      HCResult::MultipleErrors(errors) => {
        Err(HCError::Many { message: Default::default(), errors: errors })
      }
      _ => Err(HCError::UNDEFINED),
    }
  }
}

impl<T> FromResidual<Option<Infallible>> for HCResult<T> {
  #[inline]
  #[track_caller]
  fn from_residual(residual: Option<Infallible>) -> Self {
    match residual {
      Option::None => HCResult::None,
      Some(e) => HCResult::from_residual(residual),
    }
  }
}

impl<T> FromResidual<HCResult<Infallible>> for HCResult<T> {
  #[inline]
  #[track_caller]
  fn from_residual(residual: HCResult<Infallible>) -> Self {
    match residual {
      HCResult::Err(err) => HCResult::Err(err),
      HCResult::MultipleErrors(err) => HCResult::MultipleErrors(err),
      HCResult::Err(err) => HCResult::Err(err),
      _ => HCResult::None,
    }
  }
}

impl<T> Try for HCResult<T> {
  type Output = T;
  type Residual = HCResult<Infallible>;

  #[inline]
  fn from_output(output: Self::Output) -> Self {
    HCResult::Ok(output)
  }

  #[inline]
  fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
    match self {
      HCResult::Ok(v) => ControlFlow::Continue(v),
      HCResult::Err(e) => ControlFlow::Break(HCResult::Err(e)),
      HCResult::MultipleErrors(e) => ControlFlow::Break(HCResult::MultipleErrors(e)),
      HCResult::None => ControlFlow::Break(HCResult::None),
    }
  }
}

impl<T> Termination for HCResult<T> {
  fn report(self) -> std::process::ExitCode {
    match self {
      HCResult::MultipleErrors(errors) => {
        errors.stderr_print();
        ExitCode::FAILURE
      }
      HCResult::Err(error) => {
        eprintln!("{}", error);
        ExitCode::FAILURE
      }
      HCResult::None => {
        eprintln!("No Results");
        ExitCode::FAILURE
      }
      HCResult::Ok(val) => ExitCode::SUCCESS,
    }
  }
}
