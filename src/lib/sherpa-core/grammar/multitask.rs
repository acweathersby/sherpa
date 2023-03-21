pub struct WorkVerifier {
  pub(crate) complete: u32,
  pub(crate) pending:  u32,
  pub(crate) progress: u32,
}

impl WorkVerifier {
  pub fn new(pending: u32) -> Self {
    WorkVerifier { complete: 0, pending, progress: 0 }
  }

  pub fn add_units_of_work(&mut self, units_of_work: u32) {
    self.pending += units_of_work;
  }

  pub fn start_one_unit_of_work(&mut self) {
    if self.pending > 0 {
      self.pending -= 1;

      self.progress += 1;
    } else {
      panic!("No pending work")
    }
  }

  pub fn complete_one_unit_of_work(&mut self) {
    if self.progress > 0 {
      self.progress -= 1;

      self.complete += 1;
    } else {
      panic!("No work in progress")
    }
  }

  pub fn skip_one_unit_of_work(&mut self) {
    if self.pending > 0 {
      self.pending -= 1;

      self.complete += 1;
    } else {
      panic!("No pending work")
    }
  }

  pub fn is_complete(&self) -> bool {
    self.pending == 0 || self.progress == 0 || self.complete > 0
  }
}
