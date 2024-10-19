use crate::types::*;

#[derive(Clone, Copy, Default)]
pub struct OptimizationReport {
  pub start:            ComplexityMarker,
  pub end:              ComplexityMarker,
  pub canonical_rounds: usize,
}

impl OptimizationReport {
  pub fn to_string(&self) -> String {
    format!(
      "
Optimization Report 
===============================================================================
Number of Initial States   : {} 
Number of Optimized States : {} 
State Reduction            : {}% 
Complexity Reduction       : {}%
Number of canonical rounds : {}
===============================================================================",
      self.start.num_of_states,
      self.end.num_of_states,
      ((1.0 - self.end.num_of_states as f64 / self.start.num_of_states as f64) * 100.0).round(),
      ((1.0 - self.end.code_complexity / self.start.code_complexity) * 100.0).round(),
      self.canonical_rounds
    )
  }
}

#[derive(Clone, Copy, Default)]
pub struct ComplexityMarker {
  pub num_of_states:   usize,
  pub code_complexity: f64,
}

impl ComplexityMarker {
  pub fn from_map_iter<'i, I: Iterator<Item = (&'i IString, &'i Box<ParseState>)>>(db: &GrammarDatabase, states: I) -> Self {
    let (num_of_states, code_complexity) = states
      .enumerate()
      .map(|(i, (_, s))| (i, s.print(db, false).unwrap_or_default().len()))
      .fold((0, 0), |(a, c), (b, d)| (a.max(b), c + d));
    Self { num_of_states, code_complexity: code_complexity as f64 }
  }

  pub fn from_vec_iter<'i, I: Iterator<Item = &'i (IString, Box<ParseState>)>>(db: &GrammarDatabase, states: I) -> Self {
    let (num_of_states, code_complexity) = states
      .enumerate()
      .map(|(i, (_, s))| (i, s.print(db, false).unwrap_or_default().len()))
      .fold((0, 0), |(a, c), (b, d)| (a.max(b), c + d));
    Self { num_of_states, code_complexity: code_complexity as f64 }
  }

  pub fn comparison(&self, other: &Self, label: &str) -> String {
    format!(
      "Opt {} ---- 
Number of Initial States   : {} 
Number of Optimized States : {} 
State Reduction            : {}% 
Complexity Reduction       : {}%",
      label,
      self.num_of_states,
      other.num_of_states,
      ((1.0 - other.num_of_states as f64 / self.num_of_states as f64) * 100.0).round(),
      ((1.0 - other.code_complexity / self.code_complexity) * 100.0).round()
    )
  }
}
