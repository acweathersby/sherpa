//! The journal is the source for configuring the compiler, and tracking events
//!  and reporting events that occur during grammar compilation.
pub mod config;
pub mod report;
pub use self::{
  config::Config,
  report::{Report, ReportType},
};
use crate::types::*;
use std::{
  fmt::{Debug, Display},
  sync::{Arc, LockResult, RwLock},
};

#[cfg(not(feature = "wasm-target"))]
use std::time::Instant;

#[derive(Default)]
#[cfg_attr(debug_assertions, derive(Debug))]
struct ScratchPad {
  pub reports: Map<ReportType, Box<Report>>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
/// A general structure for storing and interacting with data relating to the
/// configuration, monitoring, and reporting of Sherpa commands and types.
pub struct Journal {
  config: Config,

  global_pad: Arc<RwLock<ScratchPad>>,

  scratch_pad: Box<ScratchPad>,

  active_report: Option<Box<Report>>,

  report_sink: Report,

  #[cfg(not(feature = "wasm-target"))]
  create_time: Instant,
}

impl Journal {
  /// Creates a Journal instance.
  pub fn new(config: Option<Config>) -> Journal {
    Self {
      config: config.unwrap_or_default(),
      global_pad: Arc::new(RwLock::new(Default::default())),
      scratch_pad: Default::default(),
      active_report: None,
      report_sink: Report::create_sink(),
      #[cfg(not(feature = "wasm-target"))]
      create_time: Instant::now(),
    }
  }

  /// Clone the journal, maintaining a link to the global
  /// pad to upload reports to.
  #[allow(unused)]
  pub fn transfer(&self) -> Self {
    Self {
      config: self.config.clone(),
      global_pad: self.global_pad.clone(),
      scratch_pad: Default::default(),
      active_report: None,
      report_sink: Report::create_sink(),
      #[cfg(not(feature = "wasm-target"))]
      create_time: Instant::now(),
    }
  }

  /// Get an immutable reference to the configuration settings.
  pub fn config(&self) -> &Config {
    &self.config
  }

  /// Returns a vector of all new errors that have encountered since the last
  /// time this function was called.
  pub fn errors_since_last_check() -> Option<Vec<SherpaError>> {
    None
  }

  /// Sets the active report to `report_type`, optionally creating a new report
  /// of that type if one does not already exists. Returns the previously set
  /// ReportType.
  pub fn set_active_report(&mut self, report_name: &str, report_type: ReportType) -> ReportType {
    fn set_report(p: &mut ScratchPad, n: &str, t: ReportType) -> Option<Box<Report>> {
      match p.reports.contains_key(&t) {
        true => p.reports.remove(&t),
        false => Some(Box::new(Report { name: n.to_string(), report_type: t, ..Default::default() })),
      }
    }

    self.active_report = match self.active_report.take() {
      Some(r) if r.report_type != report_type => {
        self.scratch_pad.reports.insert(r.report_type, r);
        set_report(&mut self.scratch_pad, report_name, report_type)
      }
      None => set_report(&mut self.scratch_pad, report_name, report_type),
      report => report,
    };

    self.report().report_type
  }

  /// Loads one or more reports that match `ReportType` in a closure for read
  /// access. If the closure returns `true` then no further reports are
  /// loaded. Closure is only called if matching reports can be found.
  ///
  /// Returns `true` if any reports where matched
  pub fn get_report<T: Fn(&Report) -> bool>(&self, report_type: ReportType, closure: T) -> bool {
    let mut matching_reports = false;
    for report in self.scratch_pad.reports.values() {
      if report.type_matches(report_type) {
        matching_reports = true;
        if closure(report) {
          return true;
        }
      }
    }
    match self.global_pad.read() {
      LockResult::Ok(global_pad) => {
        for report in global_pad.reports.values() {
          if report.type_matches(report_type) {
            matching_reports = true;
            if closure(report) {
              return true;
            }
          }
        }
      }
      LockResult::Err(err) => {
        eprintln!("Unable to acquire a read lock on the global pad:\n{}", err)
      }
    }
    matching_reports
  }

  /// Retrieves all reports that match the `report_type` and calls `closure` for
  /// each one, passing in the matched report as a reference.
  pub fn get_reports<T: FnMut(&Report)>(&self, report_type: ReportType, mut closure: T) {
    for report in self.scratch_pad.reports.values() {
      if report.type_matches(report_type) {
        closure(report);
      }
    }
    match self.global_pad.read() {
      LockResult::Ok(global_pad) => {
        for report in global_pad.reports.values() {
          if report.type_matches(report_type) {
            closure(report);
          }
        }
      }
      LockResult::Err(err) => {
        eprintln!("Unable to acquire a read lock on the global pad:\n{}", err)
      }
    }
  }

  #[track_caller]
  /// Get a mutable reference to the active report.
  pub fn report_mut(&mut self) -> &mut Report {
    self.active_report.as_mut().map(|r| r.as_mut()).unwrap_or_else(|| {
      #[cfg(debug_assertions)]
      eprintln!("Using mutable report sink!");
      &mut self.report_sink
    })
  }

  #[track_caller]
  /// Get a immutable reference to the active report.
  pub fn report(&self) -> &Report {
    self.active_report.as_ref().map(|r| r.as_ref()).unwrap_or_else(|| {
      #[cfg(debug_assertions)]
      eprintln!("Using report sink!");
      &self.report_sink
    })
  }

  /// Print reports that match the `discriminant` type. Returns
  /// true if any reports were printed;
  pub fn debug_print_reports(&self, discriminant: ReportType) -> bool {
    let mut printed = false;
    #[cfg(not(feature = "wasm-target"))]
    {
      let printed_mut = &mut printed;
      self.get_reports(discriminant, move |report| {
        (*printed_mut) |= true;
        println!(
          "\n{:=<80}\nReport [{}] at {:?}:\n{}\n{:=<80}",
          "",
          report.name,
          (report.create_time.duration_since(self.create_time)),
          report.debug_string(),
          ""
        )
      });
    }
    #[cfg(feature = "wasm-target")]
    {
      let printed_mut = &mut printed;
      self.get_reports(discriminant, move |report| {
        (*printed_mut) |= true;
        println!("\n{:=<80}\nReport [{}] \n{}\n{:=<80}", "", report.name, report.debug_string(), "")
      })
    }

    printed
  }

  /// Returns an array of all reports that contain errors, or None
  /// if there are no matching reports.
  pub fn get_faulty_reports(&self) -> Option<Vec<Report>> {
    let mut faulty_reports = Vec::new();
    let fr_ref = &mut faulty_reports;

    self.get_reports(ReportType::Any, move |report| {
      if report.have_errors_of_type(SherpaErrorSeverity::Critical) {
        fr_ref.push(report.clone());
      }
    });

    if faulty_reports.is_empty() {
      None
    } else {
      Some(faulty_reports)
    }
  }

  /// Prints all errors that have been generated to console.
  /// Returns `true` if any errors were reported.
  pub fn string_error_report(&self) -> Option<String> {
    let mut strings = vec![];

    self.get_reports(ReportType::Any, |report| {
      if let Some(string) = report.debug_error_string() {
        strings.push(string)
      }
    });

    if strings.is_empty() {
      None
    } else {
      Some(strings.join("\n"))
    }
  }

  /// Prints all errors that have been generated to console.
  /// Returns `true` if any errors were reported.
  pub fn debug_error_report(&self) -> bool {
    let mut errors_reported = false;
    let e_ref = &mut errors_reported;

    self.get_reports(ReportType::Any, |report| {
      (*e_ref) |= report.debug_error();
    });
    errors_reported
  }

  /// Returns true if any error in any report has a matching `severity`
  pub fn have_errors_of_type(&self, severity: SherpaErrorSeverity) -> bool {
    if !self.report().have_errors_of_type(severity) {
      for (_, report) in self.scratch_pad.reports.iter().chain(self.global_pad.read().unwrap().reports.iter()) {
        if report.have_errors_of_type(severity) {
          return true;
        }
      }
    }
    false
  }

  /// Move report data from the local pad to the global pad
  pub fn flush_reports(&mut self) {
    match self.global_pad.write() {
      LockResult::Ok(mut global_pad) => {
        // Insert reports into global pad.
        global_pad.reports.extend(self.scratch_pad.reports.drain());
        match self.active_report.take() {
          Some(report) => {
            global_pad.reports.insert(report.report_type, report);
          }
          None => {}
        }
      }
      Err(err) => {
        panic!("{}", err);
      }
    }
  }
}

impl Display for Journal {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str("Sherpa Journal")
  }
}

impl Drop for Journal {
  fn drop(&mut self) {
    // Merge thread local report data into the global_pad, but only
    // if there is more than one active journal instance.
    if (self.scratch_pad.reports.len() > 0 || self.active_report.is_some()) && Arc::strong_count(&self.global_pad) > 1 {
      self.flush_reports();
    }
  }
}

#[cfg(not(feature = "wasm-target"))]
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Timing {
  start: Instant,
  end:   Option<Instant>,
}

#[cfg(not(feature = "wasm-target"))]
impl Timing {
  #[inline(always)]
  pub fn new() -> Self {
    Timing { start: Instant::now(), end: None }
  }

  pub fn set_end(&mut self, instant: Instant) {
    self.end.get_or_insert(instant);
  }
}

#[cfg(not(feature = "wasm-target"))]
impl Debug for Timing {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if let Some(end) = self.end {
      f.write_fmt(format_args!("{:?}", (end - self.start)))
    } else {
      f.write_fmt(format_args!("Started {:?} ago", (Instant::now() - self.start)))
    }
  }
}

#[cfg(not(feature = "wasm-target"))]
impl Display for Timing {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    Debug::fmt(&self, f)
  }
}
