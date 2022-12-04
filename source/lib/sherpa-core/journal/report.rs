use super::Timing;
use crate::types::*;
use std::{collections::HashMap, time::Instant};

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
  TokenProductionCompile(ProductionId),
  ScannerCompile(ScannerId),
  OcclusionCompile,
  // The following are implemented in in other packages
  AScriptCompile,
  ByteCodeCompile,
  /// Matches all report types.
  Any,
  /// Matches all production compilation reports.
  AnyProductionCompile,
  /// Matches all IR compiling reports
  IntermediateCompile,
  // Matches the dissassembly generation report.
  Disassembly,
  ProductionCompile(ProductionId),
  ProductionCompileLR(ProductionId),
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
  pub(crate) errors:      Vec<SherpaError>,
  pub(crate) timings:     HashMap<&'static str, Timing>,
  pub(crate) create_time: Instant,
  pub(crate) error_level: SherpaErrorSeverity,
}

impl Report {
  pub(crate) fn stop_all_timers(&mut self) {
    for (_, timer) in &mut self.timings {
      if timer.is_active() {
        timer.stop();
      }
    }
  }

  pub fn have_errors_of_type(&self, severity: SherpaErrorSeverity) -> bool {
    self.error_level.intersects(severity)
  }

  pub fn get_type(&self) -> ReportType {
    self.report_type.clone()
  }

  pub fn errors(&self) -> &Vec<SherpaError> {
    &self.errors
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
    self.error_level |= error.get_severity();
    self.errors.push(error);
  }

  pub(crate) fn add_note(&mut self, note_name: &'static str, note: String) {
    self.notes.push((note_name, note));
  }

  pub(crate) fn start_timer(&mut self, timer_label: &'static str) {
    self.timings.insert(timer_label, Timing::new(timer_label));
  }

  pub(crate) fn stop_timer(&mut self, timer_label: &'static str) {
    let instant = Instant::now();

    match self.timings.entry(timer_label) {
      std::collections::hash_map::Entry::Occupied(mut e) => {
        e.get_mut().end = instant;
      }
      std::collections::hash_map::Entry::Vacant(_) => {}
    }
  }

  pub(crate) fn report_duration(&mut self, timer_label: &'static str) {
    match self.timings.entry(timer_label) {
      std::collections::hash_map::Entry::Occupied(e) => {
        let timing = e.get();
        println!("{} took {:?}", timer_label, (timing.end - timing.start))
      }
      std::collections::hash_map::Entry::Vacant(_) => {}
    }
  }

  pub(crate) fn debug_string(&self) -> String {
    format!(
      "Notes:\n{}\nTimings:\n{}\nErrors:\n{}",
      self
        .notes
        .iter()
        .map(|(label, body)| format!("---------------\n{}:\n{}", label, body))
        .collect::<Vec<_>>()
        .join("\n"),
      self
        .timings
        .iter()
        .map(|(label, body)| format!("---------------\n{}:\n{:?}", label, body))
        .collect::<Vec<_>>()
        .join("\n"),
      self.errors.iter().map(|(err)| format!("\n{}", err)).collect::<Vec<_>>().join("\n")
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
      (ScannerCompile(any), ScannerCompile(_)) if any == ScannerId::default() => true,
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
      notes:       Default::default(),
      errors:      Default::default(),
      timings:     Default::default(),
      create_time: Instant::now(),
      name:        Default::default(),
      error_level: SherpaErrorSeverity::None,
    }
  }
}
