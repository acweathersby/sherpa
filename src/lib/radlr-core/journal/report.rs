#[cfg(not(feature = "wasm-target"))]
use super::Timing;
#[allow(unused)]
use crate::types::*;
use crate::utils::create_u64_hash;
#[allow(unused)]
use std::{
  collections::{BTreeMap, HashMap},
  fmt::Display,
  hash::Hash,
  time::Instant,
};

/// Selection classes for different report types, used to filter reports
/// results.
///
/// Some values wrap an identifier type to better specifier searches. To get all
/// reports of a certain class that use an identifier, use `Default::default()`
/// (e.g. [ReportType]::GrammarCompile(Default::default())).
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum ReportType {
  /// This is a default report that is returned when an active report
  /// is not set. This will not match any report
  Sink,
  /// Matches a report of the compilation of a specific grammar, or
  /// if [GrammarId] is `GrammarId::default`, then matches all grammar
  /// compilation reports
  GrammarCompile(GrammarId),
  /// Initial parsing of a grammar source input.
  GrammarParse,
  /// TODO
  TokenNonTermCompile(NonTermId),
  /// TODO
  OcclusionCompile,
  // The following are implemented in in other packages
  /// TODO
  AScriptCompile,
  /// TODO
  ByteCodeCompile,
  /// Matches all report types.
  Any,
  /// Matches all non-terminal compilation reports.
  AnyNonTermCompile,
  /// Matches all IR compiling reports
  IntermediateCompile,
  /// Matches the dissassembly generation report.
  Disassembly,
  /// TODO
  NonTerminalCompile(DBNonTermKey),
  /// TODO
  Optimize,
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
/// Store information about a certain aspect of grammar compilation.
pub struct Report {
  pub name:        String,
  pub report_type: ReportType,
  /// At table mapping a report note label to a
  /// note body.
  pub notes:       Vec<(&'static str, String)>,
  pub _errors:     BTreeMap<u64, RadlrError>,

  #[cfg(not(feature = "wasm-target"))]
  pub timings: HashMap<&'static str, Timing>,

  #[cfg(not(feature = "wasm-target"))]
  pub create_time:    Instant,
  pub error_level:    RadlrErrorSeverity,
  pub(super) is_sink: bool,
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
  pub fn have_errors_of_type(&self, severity: RadlrErrorSeverity) -> bool {
    self.error_level == severity
  }

  /// Returns the report's type
  pub fn get_type(&self) -> ReportType {
    self.report_type.clone()
  }

  /// Returns a reference to the report's errors.
  pub fn errors(&self) -> Vec<&RadlrError> {
    self._errors.values().collect()
  }

  /// Returns the `return_val` as `RadlrResult::Ok(return_val)` if there are no
  /// errors in the active report. Otherwise returns a
  /// `RadlrResult::Err<RadlrErrors::Multi>` result.
  #[track_caller]
  pub fn wrap_ok_or_return_errors<T>(&mut self, return_val: T) -> RadlrResult<T> {
    match (self._errors.len() == 0, self.is_sink) {
      (true, false) => RadlrResult::Ok(return_val),
      (false, false) => Err(RadlrError::Multi(self.errors().into_iter().cloned().collect())),
      (_, true) => RadlrResult::Err("Invalid attempt to evaluate a report sink".into()),
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
    self.notes.iter().find(|(n, _)| (*n.to_ascii_lowercase()) == note_name.to_ascii_lowercase()).map(|(_, n)| n)
  }

  pub fn add_error<T: IntoIterator<Item = RadlrError>>(&mut self, error: T) {
    for error in error.into_iter() {
      self.error_level = error.get_severity().max(self.error_level);
      let id = create_u64_hash(&error);
      if !self._errors.contains_key(&id) {
        self._errors.insert(id, error);
      }
    }
  }

  pub fn add_note(&mut self, note_name: &'static str, note: String) {
    self.notes.push((note_name, note));
  }

  #[allow(unused)]
  pub fn start_timer(&mut self, timer_label: &'static str) {
    #[cfg(not(feature = "wasm-target"))]
    self.timings.insert(timer_label, Timing::new());
  }

  #[allow(unused)]
  pub fn stop_timer(&mut self, timer_label: &'static str) {
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

  pub fn debug_string(&self) -> String {
    #[cfg(feature = "wasm-target")]
    let timings = "";
    #[cfg(not(feature = "wasm-target"))]
    let timings =
      self.timings.iter().map(|(label, body)| format!("---------------\n{}:\n{:?}", label, body)).collect::<Vec<_>>().join("\n");

    format!(
      "Notes:\n{}\nTimings:\n{}\nErrors:\n{}",
      self.notes.iter().map(|(label, body)| format!("---------------\n{}:\n{}", label, body)).collect::<Vec<_>>().join("\n"),
      timings,
      self._errors.values().map(|err| format!("\n{}", err)).collect::<Vec<_>>().join("\n")
    )
  }

  /// Returns `true` if the type of this Report either completely or partially
  /// matches the `discriminant`.
  pub fn type_matches(&self, discriminant: ReportType) -> bool {
    use ReportType::{NonTerminalCompile as PC, TokenNonTermCompile as TPC, *};
    match (discriminant, self.report_type) {
      (GrammarCompile(any), GrammarCompile(_)) if any == GrammarId::default() => true,
      // (PC(any), PC(_)) if any == NonTermId::default() => true,
      (TPC(any), TPC(_)) if any == NonTermId::default() => true,
      // (PC(any), PC(_)) if any == NonTermId::default() => true,
      (AnyNonTermCompile, PC(_))
      | (AnyNonTermCompile, TPC(_))
      | (IntermediateCompile, TokenNonTermCompile(_))
      | (IntermediateCompile, NonTerminalCompile(_))
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
      error_level: RadlrErrorSeverity::None,
      is_sink: false,
    }
  }
}
