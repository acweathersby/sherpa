const STACK_SIZE: usize = 64;

// ///////////////////////////////////////////
// KERNEL STATE
// ///////////////////////////////////////////

pub struct ParseStack {
  pub state_stack:   [u64; STACK_SIZE],
  pub stack_pointer: i32,
}

impl ParseStack {
  #[inline]
  pub fn new() -> ParseStack {
    ParseStack { state_stack: [0; STACK_SIZE], stack_pointer: 0 }
  }

  #[inline]
  pub fn reset(&mut self, state: u32) {
    self.stack_pointer = 0;

    self.state_stack[0] = 0;

    self.push_state(state)
  }

  #[inline]
  pub fn push_state(&mut self, state: u32) {
    self.stack_pointer += 1;

    let sp = self.stack_pointer as usize;

    self.state_stack[sp] = state as u64;
  }

  #[inline]
  pub fn swap_state(&mut self, state: u32) {
    self.state_stack[self.stack_pointer as usize] = state as u64;
  }

  #[inline]
  pub fn pop_state(&mut self) -> u32 {
    if self.stack_pointer >= 0 {
      let state = self.read_state();

      self.stack_pointer -= 1;

      state
    } else {
      0
    }
  }

  #[inline]
  pub fn read_state(&self) -> u32 {
    (self.state_stack[self.stack_pointer as usize] & 0xFFFF_FFFF) as u32
  }

  #[inline]
  pub fn copy_state_stack(&self, dest: &mut ParseStack) {
    for i in 0..=self.stack_pointer {
      dest.state_stack[i as usize] = self.state_stack[i as usize];
    }
  }
}

impl Default for ParseStack {
  fn default() -> Self {
    Self::new()
  }
}
