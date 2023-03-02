#[cfg(not(feature = "wasm-target"))]
use super::Timing;

use crate::{grammar::hash_id_value_u64, types::*};
use std::{
  collections::{BTreeMap, HashMap},
  fmt::{Debug, Display},
  hash::Hash,
  time::Instant,
};

/// Selection classes for different report types, used to filter reports results.
///
/// Some values wrap an identifier type to better specifier searches. To get all
/// reports of a certain class that use an identifier, use `Default::default()`
/// (e.g. [ReportType]::GrammarCompile(Default::default())).
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReportType {
  /// This is a default report that is returned when an active report
  /// is not set. This will not match any report
  Sink,
  /// Matches a report of the compilation of a specific grammar, or
  /// if [GrammarId] is `GrammarId::default`, then matches all grammar
  /// compilation reports
  GrammarCompile(GrammarId),
  /// TODO
  TokenProductionCompile(ProductionId),
  /// TODO
  ScannerCompile(ScannerStateId),
  /// TODO
  OcclusionCompile,
  // The following are implemented in in other packages
  /// TODO
  AScriptCompile,
  /// TODO
  ByteCodeCompile,
  /// Matches all report types.
  Any,
  /// Matches all production compilation reports.
  AnyProductionCompile,
  /// Matches all IR compiling reports
  IntermediateCompile,
  /// Matches the dissassembly generation report.
  Disassembly,
  /// TODO
  ProductionCompile(ProductionId),
  /// TODO
  ProductionCompileLR(ProductionId),
  /// TODO
  Optimize,
}

#[derive(Debug, Clone)]
/// Store information about a certain aspect of grammar compilation.
pub struct Report {
  pub(crate) name:        String,
  pub(crate) report_type: ReportType,
  /// At table mapping a report note label to a
  /// note body.
  pub(crate) notes:       Vec<(&'static str, String)>,
  pub(crate) _errors:     BTreeMap<u64, SherpaError>,

  #[cfg(not(feature = "wasm-target"))]
  pub(crate) timings: HashMap<&'static str, Timing>,

  #[cfg(not(feature = "wasm-target"))]
  pub(crate) create_time: Instant,
  pub(crate) error_level: SherpaErrorSeverity,
  pub(super) is_sink:     bool,
}

impl Hash for Report {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.name.hash(state);
    self.report_type.hash(state);
    self.notes.hash(state);
    self._errors.hash(state);

    #[cfg(not(feature = "wasm-target"))]
    self.create_time.hash(state);
    self.error_level.hash(state);
  }
}

impl Report {
  ///
  pub fn display_errors(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for (_, error) in &self._errors {
      Display::fmt(&error, f)?;
    }
    std::fmt::Result::Ok(())
  }

  pub(super) fn create_sink() -> Self {
    Self { is_sink: true, ..Default::default() }
  }

  /// Returns `true` if the report contains any error with the matching severity
  pub fn have_errors_of_type(&self, severity: SherpaErrorSeverity) -> bool {
    self.error_level == severity
  }

  /// Returns the report's type
  pub fn get_type(&self) -> ReportType {
    self.report_type.clone()
  }

  /// Returns a reference to the report's errors.
  pub fn errors(&self) -> Vec<&SherpaError> {
    self._errors.values().collect()
  }

  /// Returns the `return_val` in `SherpaResult::Ok()` if there are no errors in the report,
  /// otherwise returns a `SherpResult::Err(SherpaError::Report)` type result.
  #[track_caller]
  pub fn ok_or_convert_to_error<T>(&mut self, return_val: T) -> SherpaResult<T> {
    match (self._errors.len() == 0, self.is_sink) {
      (true, false) => SherpaResult::Ok(return_val),
      (false, false) => {
        #[cfg(not(feature = "wasm-target"))]
        {
          let instant = Instant::now();
          for timer in self.timings.values_mut() {
            timer.set_end(instant)
          }
        }
        SherpaResult::Err((&*self).into())
      }
      (_, true) => SherpaResult::Err("Invalid attempt to evaluate a report sink".into()),
    }
  }

  /// Prints all errors to stdout and returns `true`
  /// if any errors were printed.
  pub fn debug_error(&self) -> bool {
    let errors = self.errors();
    if errors.len() > 0 {
      println!("\n{:=<80}\nReport [{}] errors:", "", self.name);
      for err in self.errors() {
        println!("{}", err);
      }
      true
    } else {
      false
    }
  }

  /// Get a string of all errors encountered in the report
  pub fn debug_error_string(&self) -> Option<String> {
    let errors = self.errors();
    if errors.len() > 0 {
      let mut string = vec![format!("\n{:=<80}\nReport [{}] errors:", "", self.name)];

      for err in self.errors() {
        string.push(err.to_string());
      }

      Some(string.join("\n"))
    } else {
      None
    }
  }

  /// Get contents of the first note whose name matches `note_name`
  pub fn get_note<'a>(&'a self, note_name: &str) -> Option<&'a String> {
    self
      .notes
      .iter()
      .find(|(n, _)| (*n.to_ascii_lowercase()) == note_name.to_ascii_lowercase())
      .map(|(_, n)| n)
  }

  pub(crate) fn add_error(&mut self, error: SherpaError) {
    self.error_level = error.get_severity().max(self.error_level);
    let id = hash_id_value_u64(&error);
    if !self._errors.contains_key(&id) {
      self._errors.insert(id, error);
    }
  }

  pub(crate) fn add_note(&mut self, note_name: &'static str, note: String) {
    self.notes.push((note_name, note));
  }

  pub(crate) fn start_timer(&mut self, timer_label: &'static str) {
    #[cfg(not(feature = "wasm-target"))]
    self.timings.insert(timer_label, Timing::new());
  }

  pub(crate) fn stop_timer(&mut self, timer_label: &'static str) {
    #[cfg(not(feature = "wasm-target"))]
    {
      let instant = Instant::now();

      match self.timings.entry(timer_label) {
        std::collections::hash_map::Entry::Occupied(mut e) => {
          e.get_mut().set_end(instant);
        }
        std::collections::hash_map::Entry::Vacant(_) => {}
      }
    }
  }

  pub(crate) fn debug_string(&self) -> String {
    #[cfg(feature = "wasm-target")]
    let timings = "";
    #[cfg(not(feature = "wasm-target"))]
    let timings = self
      .timings
      .iter()
      .map(|(label, body)| format!("---------------\n{}:\n{:?}", label, body))
      .collect::<Vec<_>>()
      .join("\n");

    format!(
      "Notes:\n{}\nTimings:\n{}\nErrors:\n{}",
      self
        .notes
        .iter()
        .map(|(label, body)| format!("---------------\n{}:\n{}", label, body))
        .collect::<Vec<_>>()
        .join("\n"),
      timings,
      self._errors.values().map(|err| format!("\n{}", err)).collect::<Vec<_>>().join("\n")
    )
  }

  /// Returns `true` if the type of this Report either completely or partially
  /// matches the `discriminant`.
  pub fn type_matches(&self, discriminant: ReportType) -> bool {
    use ReportType::{
      ProductionCompile as PC,
      ProductionCompileLR as PC_LR,
      TokenProductionCompile as TPC,
      *,
    };
    match (discriminant, self.report_type) {
      (ScannerCompile(any), ScannerCompile(_)) if any == ScannerStateId::default() => true,
      (GrammarCompile(any), GrammarCompile(_)) if any == GrammarId::default() => true,
      (PC_LR(any), PC_LR(_)) if any == ProductionId::default() => true,
      (TPC(any), TPC(_)) if any == ProductionId::default() => true,
      (PC(any), PC(_)) if any == ProductionId::default() => true,

      (AnyProductionCompile, PC(_))
      | (AnyProductionCompile, PC_LR(_))
      | (AnyProductionCompile, TPC(_))
      | (IntermediateCompile, TokenProductionCompile(_))
      | (IntermediateCompile, ScannerCompile(_))
      | (IntermediateCompile, ProductionCompile(_))
      | (AScriptCompile, AScriptCompile)
      | (ByteCodeCompile, ByteCodeCompile)
      | (OcclusionCompile, OcclusionCompile) => true,
      (a, b) if a == b => true,
      (Any, _) => true,
      _ => false,
    }
  }
}

impl Default for Report {
  fn default() -> Self {
    Self {
      report_type: ReportType::Sink,
      notes: Default::default(),
      _errors: Default::default(),
      #[cfg(not(feature = "wasm-target"))]
      timings: Default::default(),
      #[cfg(not(feature = "wasm-target"))]
      create_time: Instant::now(),
      name: Default::default(),
      error_level: SherpaErrorSeverity::None,
      is_sink: false,
    }
  }
}
