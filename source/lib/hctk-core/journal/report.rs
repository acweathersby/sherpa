use super::Timing;
use crate::types::*;
use std::{collections::HashMap, time::Instant};

/// Classes for different report types. Some values can support Identifier types
/// to better refine report searches. Do get all reports of a certain class that
/// use Identifier, use `Default::default()` (e.g. `ReportType::GrammarCompile(Default::default())`).
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReportType {
  /// This is a default report that is returned when an active report
  /// is not set.
  Sink,
  GrammarCompile(GrammarId),
  TokenProductionCompile(ProductionId),
  ScannerCompile(ScannerId),
  OcclusionCompile,
  // The following are implemented in in other packages
  AScriptCompile,
  ByteCodeCompile,
  /// Custom report classes
  Any,
  IntermediateCompile,
  Disassembly,
  ProductionCompile(ProductionId),
  Optimize,
}

#[derive(Debug, Clone)]
/// Store information about a certain aspect of grammar compilation.
pub struct Report {
  pub name:        String,
  pub report_type: ReportType,
  /// At table mapping a report note label to a
  /// note body.
  pub notes:       Vec<(&'static str, String)>,
  pub errors:      Vec<HCError>,
  pub timings:     HashMap<&'static str, Timing>,
  pub create_time: Instant,
  pub error_level: HCErrorSeverity,
}

impl Report {
  pub fn have_errors_of_type(&self, severity: HCErrorSeverity) -> bool {
    self.error_level.intersects(severity)
  }

  pub fn get_type(&self) -> ReportType {
    self.report_type.clone()
  }

  pub fn errors(&self) -> &Vec<HCError> {
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

  pub fn add_error(&mut self, error: HCError) {
    self.error_level |= error.get_severity();
    self.errors.push(error);
  }

  pub fn add_note(&mut self, note_name: &'static str, note: String) {
    self.notes.push((note_name, note));
  }

  pub fn start_timer(&mut self, timer_label: &'static str) {
    self.timings.insert(timer_label, Timing::new(timer_label));
  }

  pub fn stop_timer(&mut self, timer_label: &'static str) {
    let instant = Instant::now();

    match self.timings.entry(timer_label) {
      std::collections::hash_map::Entry::Occupied(mut e) => {
        e.get_mut().end = instant;
      }
      std::collections::hash_map::Entry::Vacant(_) => {}
    }
  }

  pub fn report_duration(&mut self, timer_label: &'static str) {
    match self.timings.entry(timer_label) {
      std::collections::hash_map::Entry::Occupied(e) => {
        let timing = e.get();
        println!("{} took {:?}", timer_label, (timing.end - timing.start))
      }
      std::collections::hash_map::Entry::Vacant(_) => {}
    }
  }

  pub fn debug_string(&self) -> String {
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

  pub fn type_matches(&self, discriminant: ReportType) -> bool {
    use ReportType::{ProductionCompile as PC, TokenProductionCompile as TPC, *};
    match (discriminant, self.report_type) {
      (ScannerCompile(any), ScannerCompile(_)) if any == ScannerId::default() => true,
      (GrammarCompile(any), GrammarCompile(_)) if any == GrammarId::default() => true,
      (TPC(any), TPC(_)) if any == ProductionId::default() => true,
      (PC(any), PC(_)) if any == ProductionId::default() => true,
      (IntermediateCompile, TokenProductionCompile(_))
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
      error_level: HCErrorSeverity::None,
    }
  }
}
