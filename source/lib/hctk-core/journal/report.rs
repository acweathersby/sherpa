use super::Timing;
use crate::types::*;
use std::{collections::HashMap, time::Instant};

#[derive(Debug, Clone, Copy)]
pub enum ReportType {
  /// This is a default report that is returned when an active report
  /// is not set.
  Sink,
  GrammarCompile,
  ProductionCompile,
  ScannerCompile,
  OcclusionCompile,
  // The following are implemented in in other packages
  AScriptCompile,
  ByteCodeCompile,
}
#[derive(Debug, Clone)]
/// Store information about a certain aspect of grammar compilation.
pub struct Report {
  pub(super) report_type: ReportType,
  /// At table mapping a report note label to a
  /// note body.
  pub(super) notes:       HashMap<&'static str, String>,
  pub(super) errors:      Vec<HCError>,
  pub(super) timings:     HashMap<&'static str, Timing>,
  pub(super) create_time: Instant,
}

impl Report {
  pub fn get_type(&self) -> ReportType {
    self.report_type
  }

  pub fn errors(&self) -> &Vec<HCError> {
    &self.errors
  }

  pub fn add_error(&mut self, error: HCError) {
    self.errors.push(error);
  }

  pub fn add_note(&mut self, note_name: &'static str, note: String) {
    self.notes.insert(note_name, note);
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
    let instant = Instant::now();

    match self.timings.entry(timer_label) {
      std::collections::hash_map::Entry::Occupied(mut e) => {
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
}

impl Default for Report {
  fn default() -> Self {
    Self {
      report_type: ReportType::Sink,
      notes:       Default::default(),
      errors:      Default::default(),
      timings:     Default::default(),
      create_time: Instant::now(),
    }
  }
}
