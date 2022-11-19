//! Global catch-all for errors.
//! Methods for reporting errors are provided

use std::convert;
use std::fmt::Display;
use std::fmt::Error;
use std::ops::ControlFlow;
use std::ops::FromResidual;
use std::ops::Residual;
use std::ops::Try;
use std::path::Path;
use std::path::PathBuf;

use crate::types::*;

/// A Result type that uses the HCTKError enum.
// pub type HCResult<T> = Result<T, HCError>;

#[derive(PartialEq, Debug, Hash)]
pub enum HCResult<T> {
  Ok(T),
  Err(HCError),
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
  pub fn is_err(&self) -> bool {
    matches!(self, HCResult::Err(_))
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
  pub fn is_faulty(&self) -> bool {
    self.is_err() || self.is_none()
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

impl<T, E> FromResidual<Result<convert::Infallible, E>> for HCResult<T>
where
  HCError: From<E>,
{
  #[inline]
  #[track_caller]
  fn from_residual(residual: Result<convert::Infallible, E>) -> Self {
    match residual {
      Err(e) => HCResult::Err(HCError::from(e)),
      _ => HCResult::from_residual(residual),
    }
  }
}

impl<T> FromResidual<Option<convert::Infallible>> for HCResult<T> {
  #[inline]
  #[track_caller]
  fn from_residual(residual: Option<convert::Infallible>) -> Self {
    match residual {
      Option::None => HCResult::None,
      Some(e) => HCResult::from_residual(residual),
    }
  }
}

impl<T> FromResidual<HCResult<T>> for HCResult<T> {
  #[inline]
  #[track_caller]
  fn from_residual(residual: HCResult<T>) -> Self {
    residual
  }
}

impl<T> Residual<T> for HCResult<T> {
  type TryType = HCResult<T>;
}

impl<T> Try for HCResult<T> {
  type Output = T;
  type Residual = HCResult<T>;

  #[inline]
  fn from_output(output: Self::Output) -> Self {
    HCResult::Ok(output)
  }

  #[inline]
  fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
    match self {
      HCResult::Ok(v) => ControlFlow::Continue(v),
      HCResult::Err(e) => ControlFlow::Break(HCResult::Err(e)),
      HCResult::None => ControlFlow::Break(HCResult::None),
    }
  }
}
