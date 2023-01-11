//! The journal is the source for configuring the compiler, and tracking events
//!  and reporting events that occur during grammar compilation.
pub mod config;
pub mod report;
pub use self::{
  config::{Config, ParseAlgorithm},
  report::{Report, ReportType},
};
use crate::{
  intermediate::{construct_recursive_descent, utils::generate_scanner_symbol_items},
  types::*,
};
use std::{
  collections::{BTreeSet, HashMap},
  fmt::{Debug, Display},
  sync::{Arc, LockResult, RwLock},
  time::Instant,
};

#[derive(Default, Debug)]
struct ScratchPad {
  pub occluding_symbols: HashMap<SymbolID, SymbolSet>,
  pub occlusion_tracking: bool,
  pub reports: HashMap<ReportType, Box<Report>>,
}

#[derive(Debug)]
/// A general structure for storing and interacting with data relating to the configuration,
/// monitoring, and reporting of Sherpa commands and types.
pub struct Journal {
  /// A fully compiled grammar.
  grammar: Option<Arc<GrammarStore>>,

  config: Config,

  global_pad: Arc<RwLock<ScratchPad>>,

  scratch_pad: Box<ScratchPad>,

  occluding_symbols: Option<Arc<HashMap<SymbolID, SymbolSet>>>,

  active_report: Option<Box<Report>>,

  report_sink: Report,

  create_time: Instant,
}
impl Journal {
  /// Creates a Journal instance.
  pub fn new(config: Option<Config>) -> Journal {
    Self {
      grammar: None,
      config: config.unwrap_or_default(),
      global_pad: Arc::new(RwLock::new(Default::default())),
      scratch_pad: Default::default(),
      occluding_symbols: None,
      active_report: None,
      report_sink: Default::default(),
      create_time: Instant::now(),
    }
  }

  pub(crate) fn transfer(&self) -> Self {
    Self {
      grammar: self.grammar.clone(),
      config: self.config.clone(),
      global_pad: self.global_pad.clone(),
      scratch_pad: Default::default(),
      occluding_symbols: self.occluding_symbols.clone(),
      active_report: None,
      report_sink: Default::default(),
      create_time: Instant::now(),
    }
  }

  /// Get an immutable reference to the configuration settings.
  pub fn config(&self) -> &Config {
    &self.config
  }

  /// Returns a vector of all new errors that have encountered since the last
  /// time this function was called.
  pub fn errors_since_last_check() -> SherpaResult<Vec<SherpaError>> {
    SherpaResult::None
  }

  /// Sets the active report to `report_type`, optionally creating a new report of that type
  /// if one does not already exists. Returns the previously set ReportType.
  pub(crate) fn set_active_report(
    &mut self,
    report_name: &str,
    report_type: ReportType,
  ) -> ReportType {
    fn set_report(p: &mut ScratchPad, n: &str, t: ReportType) -> Option<Box<Report>> {
      match p.reports.contains_key(&t) {
        true => p.reports.remove(&t),
        false => {
          Some(Box::new(Report { name: n.to_string(), report_type: t, ..Default::default() }))
        }
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

  /// Loads a report in an closure for read access. Closure is only called
  /// if the report can be found. Returns `true` if any reports where matched
  pub fn get_report<T: Fn(&Report)>(&self, report_type: ReportType, closure: T) -> bool {
    let mut have_matches = false;
    match self.scratch_pad.reports.get(&report_type) {
      Some(report) => {
        have_matches = true;
        closure(report.as_ref());
      }
      _ => match self.global_pad.read() {
        LockResult::Ok(global_pad) => match global_pad.reports.get(&report_type) {
          Some(report) => {
            have_matches = true;
            closure(report.as_ref());
          }
          _ => {}
        },
        _ => {}
      },
    }
    have_matches
  }

  /// Retrieves all reports that match the `report_type` and calls `closure` for each
  /// one, passing in the matched report as a reference.
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

  /// Get a mutable reference to the active report.
  pub(crate) fn report_mut(&mut self) -> &mut Report {
    self.active_report.as_mut().map(|r| r.as_mut()).unwrap_or(&mut self.report_sink)
  }

  /// Get a immutable reference to the active report.
  pub fn report(&self) -> &Report {
    self.active_report.as_ref().map(|r| r.as_ref()).unwrap_or(&self.report_sink)
  }

  /// Print reports that match the `discriminant` type.
  pub fn debug_print_reports(&self, discriminant: ReportType) {
    self.get_reports(discriminant, |report| {
      eprintln!(
        "\n{:=<80}\nReport [{}] at {:?}:\n{}\n{:=<80}",
        "",
        report.name,
        (report.create_time.duration_since(self.create_time)),
        report.debug_string(),
        ""
      )
    });
  }

  /// Prints all errors that have been generated to console.
  /// Returns `true` if any errors were reported.
  pub fn debug_error_report(&self) -> bool {
    let mut errors_reported = false;
    let e_ref = &mut errors_reported;

    self.get_reports(ReportType::Any, |report| {
      let errors = report.errors();
      if errors.len() > 0 {
        (*e_ref) = true;
        eprintln!("\n{:=<80}\nReport [{}] errors:", "", report.name);
        for err in report.errors() {
          eprintln!("{}", err);
        }
      }
    });
    errors_reported
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

/// Grammar And Symbol Occlusions
impl Journal {
  pub(crate) fn occlusion_tracking_mode(&self) -> bool {
    self.scratch_pad.occlusion_tracking
  }

  pub(crate) fn set_grammar(&mut self, grammar: Arc<GrammarStore>) {
    self.grammar = Some(grammar.clone());
    if self.config.allow_occluding_symbols {
      self.set_active_report("Occlusion Compilation", ReportType::OcclusionCompile);

      self.report_mut().start_timer("Symbol Occlusion");

      let symbols = grammar
        .symbols
        .iter()
        .filter_map(|(id, sym)| if sym.scanner_only { None } else { Some(id) })
        .cloned()
        .collect::<BTreeSet<_>>();

      let items = generate_scanner_symbol_items(symbols, self);

      self.scratch_pad.occlusion_tracking = true;

      match construct_recursive_descent(self, ScanType::ScannerEntry, &items) {
        SherpaResult::Ok(_) => {
          self.occluding_symbols = Some(Arc::new(self.scratch_pad.occluding_symbols.clone()));
        }
        SherpaResult::Err(err) => {
          self.report_mut().add_error(err);
        }
        // Only expecting single errors to be ejected by these functions.
        _ => {}
      };

      self.scratch_pad.occluding_symbols.clear();

      self.scratch_pad.occlusion_tracking = false;

      self.report_mut().stop_timer("Symbol Occlusion");

      self.report_mut().report_duration("Symbol Occlusion");

      self.flush_reports();
    }
  }

  pub fn grammar<'b>(&'b self) -> Option<Arc<GrammarStore>> {
    self.grammar.iter().cloned().next()
  }

  pub(crate) fn add_occlusions(&mut self, occluding_symbols: SymbolSet) {
    for sym_a in &occluding_symbols {
      let table =
        self.scratch_pad.occluding_symbols.entry(*sym_a).or_insert_with(|| BTreeSet::new());

      for sym_b in &occluding_symbols {
        if sym_a == sym_b {
          continue;
        }
        // The idea here is to add symbols with lower precedence to the occlusion table
        // of symbols with higher precedence. For example, given this grammar
        // ```
        // <> A > \funct \(
        //    |   tk:id  \{
        //
        // <> id > g:id(+)
        // ```
        // The DefinedSymbol `\funct` has a higher precedence then TokenProduction symbol `tk:id`.
        // When using the occlusion table, we force the compiler to consider the symbols as the
        // "same", which should then cause it to generate a peek state that uses the following
        // symbols [ \( & \{ ] to resolve the conflict.
        match (sym_a, sym_b) {
          (sym, SymbolID::TokenProduction(..)) if sym.is_defined() => {
            table.insert(*sym_b);
          }
          _ => {}
        };
      }
    }
  }

  pub(crate) fn get_occlusion_table<'b>(&'b self) -> &'b HashMap<SymbolID, SymbolSet> {
    match self.occluding_symbols.as_ref() {
      Some(oc) => oc,
      None => &self.scratch_pad.occluding_symbols,
    }
  }
}

impl Drop for Journal {
  fn drop(&mut self) {
    // Merge thread local report data into the global_pad, but only
    // if there is more than one active journal instance.
    if (self.scratch_pad.reports.len() > 0 || self.active_report.is_some())
      && Arc::strong_count(&self.global_pad) > 1
    {
      self.flush_reports();
    }
  }
}

#[derive(Clone, Copy)]
pub(super) struct Timing {
  label:  &'static str,
  start:  Instant,
  end:    Instant,
  active: bool,
}

impl Timing {
  #[inline(always)]
  pub fn new(label: &'static str) -> Self {
    Timing { label, start: Instant::now(), end: Instant::now(), active: true }
  }

  #[inline(always)]
  pub fn stop(&mut self) {
    self.end = Instant::now();
    self.active = false;
  }

  #[inline(always)]
  pub fn is_active(&self) -> bool {
    self.active
  }
}

impl Debug for Timing {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!("{:?}", (self.end - self.start)))
  }
}

impl Display for Timing {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    Debug::fmt(&self, f)
  }
}
